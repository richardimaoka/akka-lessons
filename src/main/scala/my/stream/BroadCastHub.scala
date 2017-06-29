package my.stream

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{Outlet, _}
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Sink, Source}
import akka.stream.stage.{OutHandler, _}
import akka.stream.testkit.TestSubscriber

import scala.concurrent.duration._
import my.wrapper.Wrapper

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success, Try}

class MyBroadcastHub[T](bufferSize: Int) extends GraphStageWithMaterializedValue[SinkShape[T], Source[T, NotUsed]] {
  require(bufferSize > 0, "Buffer size must be positive")
  require(bufferSize < 4096, "Buffer size larger then 4095 is not allowed")
  require((bufferSize & bufferSize - 1) == 0, "Buffer size must be a power of two")

  private val Mask = bufferSize - 1
  private val WheelMask = (bufferSize * 2) - 1

  val in: Inlet[T] = Inlet("BroadcastHub.in")
  override val shape: SinkShape[T] = SinkShape(in)

  // Half of buffer size, rounded up
  private[this] val DemandThreshold = (bufferSize / 2) + (bufferSize % 2)

  private sealed trait HubEvent

  private object RegistrationPending extends HubEvent
  private final case class UnRegister(id: Long, previousOffset: Int, finalOffset: Int) extends HubEvent
  private final case class Advance(id: Long, previousOffset: Int) extends HubEvent
  private final case class NeedWakeup(id: Long, previousOffset: Int, currentOffset: Int) extends HubEvent

  private final case class Consumer(id: Long, callback: AsyncCallback[ConsumerEvent])

  private object Completed

  private sealed trait HubState
  private case class Open(callbackFuture: Future[AsyncCallback[HubEvent]], registrations: List[Consumer]) extends HubState
  private case class Closed(failure: Option[Throwable]) extends HubState

  private class BroadcastSinkLogic(_shape: Shape)
    extends GraphStageLogic(_shape) with InHandler {

    private[this] val callbackPromise: Promise[AsyncCallback[HubEvent]] = Promise()
    private[this] val noRegistrationsState = Open(callbackPromise.future, Nil)
    val state = new AtomicReference[HubState](noRegistrationsState)

    // Start from values that will almost immediately overflow. This has no effect on performance, any starting
    // number will do, however, this protects from regressions as these values *almost surely* overflow and fail
    // tests if someone makes a mistake.
    @volatile private[this] var tail = Int.MaxValue
    private[this] var head = Int.MaxValue
    /*
     * An Array with a published tail ("latest message") and a privately maintained head ("earliest buffered message").
     * Elements are published by simply putting them into the array and bumping the tail. If necessary, certain
     * consumers are sent a wakeup message through an AsyncCallback.
     */
    private[this] val queue = new Array[AnyRef](bufferSize)
    /* This is basically a classic Bucket Queue: https://en.wikipedia.org/wiki/Bucket_queue
     * (in fact, this is the variant described in the Optimizations section, where the given set
     * of priorities always fall to a range
     *
     * This wheel tracks the position of Consumers relative to the slowest ones. Every slot
     * contains a list of Consumers being known at that location (this might be out of date!).
     * Consumers from time to time send Advance messages to indicate that they have progressed
     * by reading from the broadcast queue. Consumers that are blocked (due to reaching tail) request
     * a wakeup and update their position at the same time.
     *
     */
    private[this] val consumerWheel = Array.fill[List[Consumer]](bufferSize * 2)(Nil)
    private[this] var activeConsumers = 0

    override def preStart(): Unit = {
      setKeepGoing(true)
      callbackPromise.success(getAsyncCallback[HubEvent](onEvent))
      println(s"MyBroadcastHub: pull from preStart()")
      pull(in)
    }

    // Cannot complete immediately if there is no space in the queue to put the completion marker
    override def onUpstreamFinish(): Unit = {
      if (!isFull) {
        println(s"MyBroadcastHub: complete")
        complete()
      }
    }

    override def onPush(): Unit = {
      val elem = grab(in)
      println(s"MyBroadcastHub: onPush(${elem})")
      publish(elem)
      if (!isFull) {
        println(s"MyBroadcastHub: pull in onPush()")
        pull(in)
      }
    }

    private def onEvent(ev: HubEvent): Unit = {
      println(s"MyBroadcastHub: onEvent(${ev})")
      ev match {
        case RegistrationPending ⇒
          state.getAndSet(noRegistrationsState).asInstanceOf[Open].registrations foreach { consumer ⇒
            val startFrom = head
            activeConsumers += 1
            addConsumer(consumer, startFrom)
            consumer.callback.invoke(Initialize(startFrom))
          }

        case UnRegister(id, previousOffset, finalOffset) ⇒
          activeConsumers -= 1
          val consumer = findAndRemoveConsumer(id, previousOffset)
          if (activeConsumers == 0) {
            if (isClosed(in)) completeStage()
            else if (head != finalOffset) {
              // If our final consumer goes away, we roll forward the buffer so a subsequent consumer does not
              // see the already consumed elements. This feature is quite handy.
              while (head != finalOffset) {
                queue(head & Mask) = null
                head += 1
              }
              head = finalOffset
              if (!hasBeenPulled(in)) pull(in)
            }
          } else checkUnblock(previousOffset)
        case Advance(id, previousOffset) ⇒
          val newOffset = previousOffset + DemandThreshold
          // Move the consumer from its last known offest to its new one. Check if we are unblocked.
          val consumer = findAndRemoveConsumer(id, previousOffset)
          addConsumer(consumer, newOffset)
          checkUnblock(previousOffset)
        case NeedWakeup(id, previousOffset, currentOffset) ⇒
          // Move the consumer from its last known offest to its new one. Check if we are unblocked.
          val consumer = findAndRemoveConsumer(id, previousOffset)
          addConsumer(consumer, currentOffset)

          // Also check if the consumer is now unblocked since we published an element since it went asleep.
          if (currentOffset != tail) consumer.callback.invoke(Wakeup)
          checkUnblock(previousOffset)
      }
    }

    // Producer API
    // We are full if the distance between the slowest (known) consumer and the fastest (known) consumer is
    // the buffer size. We must wait until the slowest either advances, or cancels.
    private def isFull: Boolean = tail - head == bufferSize

    override def onUpstreamFailure(ex: Throwable): Unit = {
      val failMessage = HubCompleted(Some(ex))

      // Notify pending consumers and set tombstone
      state.getAndSet(Closed(Some(ex))).asInstanceOf[Open].registrations foreach { consumer ⇒
        consumer.callback.invoke(failMessage)
      }

      // Notify registered consumers
      consumerWheel.iterator.flatMap(_.iterator) foreach { consumer ⇒
        consumer.callback.invoke(failMessage)
      }
      failStage(ex)
    }

    /*
     * This method removes a consumer with a given ID from the known offset and returns it.
     *
     * NB: You cannot remove a consumer without knowing its last offset! Consumers on the Source side always must
     * track this so this can be a fast operation.
     */
    private def findAndRemoveConsumer(id: Long, offset: Int): Consumer = {
      // TODO: Try to eliminate modulo division somehow...
      val wheelSlot = offset & WheelMask
      var consumersInSlot = consumerWheel(wheelSlot)
      //debug(s"consumers before removal $consumersInSlot")
      var remainingConsumersInSlot: List[Consumer] = Nil
      var removedConsumer: Consumer = null

      while (consumersInSlot.nonEmpty) {
        val consumer = consumersInSlot.head
        if (consumer.id != id) remainingConsumersInSlot = consumer :: remainingConsumersInSlot
        else removedConsumer = consumer
        consumersInSlot = consumersInSlot.tail
      }
      consumerWheel(wheelSlot) = remainingConsumersInSlot
      removedConsumer
    }

    /*
     * After removing a Consumer from a wheel slot (because it cancelled, or we moved it because it advanced)
     * we need to check if it was blocking us from advancing (being the slowest).
     */
    private def checkUnblock(offsetOfConsumerRemoved: Int): Unit = {
      if (unblockIfPossible(offsetOfConsumerRemoved)) {
        if (isClosed(in)) complete()
        else if (!hasBeenPulled(in)) pull(in)
      }
    }

    private def unblockIfPossible(offsetOfConsumerRemoved: Int): Boolean = {
      var unblocked = false
      if (offsetOfConsumerRemoved == head) {
        // Try to advance along the wheel. We can skip any wheel slots which have no waiting Consumers, until
        // we either find a nonempty one, or we reached the end of the buffer.
        while (consumerWheel(head & WheelMask).isEmpty && head != tail) {
          queue(head & Mask) = null
          head += 1
          unblocked = true
        }
      }
      unblocked
    }

    private def addConsumer(consumer: Consumer, offset: Int): Unit = {
      val slot = offset & WheelMask
      consumerWheel(slot) = consumer :: consumerWheel(slot)
    }

    /*
     * Send a wakeup signal to all the Consumers at a certain wheel index. Note, this needs the actual index,
     * which is offset modulo (bufferSize + 1).
     */
    private def wakeupIdx(idx: Int): Unit = {
      val itr = consumerWheel(idx).iterator
      while (itr.hasNext) itr.next().callback.invoke(Wakeup)
    }

    private def complete(): Unit = {
      val idx = tail & Mask
      val wheelSlot = tail & WheelMask
      queue(idx) = Completed
      wakeupIdx(wheelSlot)
      tail = tail + 1
      if (activeConsumers == 0) {
        // Existing consumers have already consumed all elements and will see completion status in the queue
        completeStage()
      }
    }

    override def postStop(): Unit = {
      // Notify pending consumers and set tombstone

      @tailrec def tryClose(): Unit = state.get() match {
        case Closed(_) ⇒ // Already closed, ignore
        case open: Open ⇒
          if (state.compareAndSet(open, Closed(None))) {
            val completedMessage = HubCompleted(None)
            open.registrations foreach { consumer ⇒
              consumer.callback.invoke(completedMessage)
            }
          } else tryClose()
      }

      tryClose()
    }

    private def publish(elem: T): Unit = {
      println(s"MyBroadcastHub: publish ${elem}")
      val idx = tail & Mask
      val wheelSlot = tail & WheelMask
      queue(idx) = elem.asInstanceOf[AnyRef]
      // Publish the new tail before calling the wakeup
      tail = tail + 1
      wakeupIdx(wheelSlot)
    }

    // Consumer API
    def poll(offset: Int): AnyRef = {
      println(s"MyBroadcastHub: poll (offset =${offset})")
      if (offset == tail) null
      else queue(offset & Mask)
    }

    setHandler(in, this)

  }

  private sealed trait ConsumerEvent
  private object Wakeup extends ConsumerEvent
  private final case class HubCompleted(failure: Option[Throwable]) extends ConsumerEvent
  private final case class Initialize(offset: Int) extends ConsumerEvent

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Source[T, NotUsed]) = {
    val idCounter = new AtomicLong()

    val logic = new BroadcastSinkLogic(shape)

    val source = new GraphStage[SourceShape[T]] {
      val out: Outlet[T] = Outlet("BroadcastHub.out")
      override val shape: SourceShape[T] = SourceShape(out)

      override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) with OutHandler {
        private[this] var untilNextAdvanceSignal = DemandThreshold
        private[this] val id = idCounter.getAndIncrement()
        private[this] var offsetInitialized = false
        private[this] var hubCallback: AsyncCallback[HubEvent] = _

        /*
         * We need to track our last offset that we published to the Hub. The reason is, that for efficiency reasons,
         * the Hub can only look up and move/remove Consumers with known wheel slots. This means that no extra hash-map
         * is needed, but it also means that we need to keep track of both our current offset, and the last one that
         * we published.
         */
        private[this] var previousPublishedOffset = 0
        private[this] var offset = 0

        override def preStart(): Unit = {
          println(s"createLogic preStart(${id}) for ${inheritedAttributes}")
          val callback = getAsyncCallback(onCommand)

          val onHubReady: Try[AsyncCallback[HubEvent]] ⇒ Unit = {
            case Success(callback) ⇒
              hubCallback = callback
              if (isAvailable(out) && offsetInitialized) onPull()
              callback.invoke(RegistrationPending)
            case Failure(ex) ⇒
              failStage(ex)
          }

          @tailrec def register(): Unit = {
            val state = logic.state.get()
            println(s"register() called for ${state}")
            state match {
              case Closed(Some(ex)) ⇒ failStage(ex)
              case Closed(None)     ⇒ completeStage()
              case previousState @ Open(callbackFuture, registrations) ⇒
                val newRegistrations = Consumer(id, callback) :: registrations
                if (logic.state.compareAndSet(previousState, Open(callbackFuture, newRegistrations))) {
                  callbackFuture.onComplete(getAsyncCallback(onHubReady).invoke)(materializer.executionContext)
                } else register()
            }
          }

          /*
           * Note that there is a potential race here. First we add ourselves to the pending registrations, then
           * we send RegistrationPending. However, another downstream might have triggered our registration by its
           * own RegistrationPending message, since we are in the list already.
           * This means we might receive an onCommand(Initialize(offset)) *before* onHubReady fires so it is important
           * to only serve elements after both offsetInitialized = true and hubCallback is not null.
           */
          register()

        }

        override def onPull(): Unit = {

          if (offsetInitialized && (hubCallback ne null)) {
            val elem = logic.poll(offset)
            println(s"onPull() called from createLogic() for ${elem}")

            elem match {
              case null ⇒
                hubCallback.invoke(NeedWakeup(id, previousPublishedOffset, offset))
                previousPublishedOffset = offset
                untilNextAdvanceSignal = DemandThreshold
              case Completed ⇒
                completeStage()
              case _ ⇒
                push(out, elem.asInstanceOf[T])
                offset += 1
                untilNextAdvanceSignal -= 1
                if (untilNextAdvanceSignal == 0) {
                  untilNextAdvanceSignal = DemandThreshold
                  val previousOffset = previousPublishedOffset
                  previousPublishedOffset += DemandThreshold
                  hubCallback.invoke(Advance(id, previousOffset))
                }
            }
          }
          else
            println(s"onPull() called from createLogic() but condition is not satisified")
        }

        override def postStop(): Unit = {
          println("postStop() called from createLogic()")
          if (hubCallback ne null)
            hubCallback.invoke(UnRegister(id, previousPublishedOffset, offset))
        }

        private def onCommand(cmd: ConsumerEvent): Unit = cmd match {
          case HubCompleted(Some(ex)) ⇒ failStage(ex)
          case HubCompleted(None)     ⇒ completeStage()
          case Wakeup ⇒
            if (isAvailable(out)) onPull()
          case Initialize(initialOffset) ⇒
            offsetInitialized = true
            previousPublishedOffset = initialOffset
            offset = initialOffset
            if (isAvailable(out) && (hubCallback ne null)) onPull()
        }

        setHandler(out, this)
      }
    }

    (logic, Source.fromGraph(source))
  }
}

object BroadCastTest {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
    // A simple producer that publishes a new "message" every second
    val producer = Source.tick(20 milliseconds, 50 microseconds, "New message")

    // Attach a BroadcastHub Sink to the producer. This will materialize to a
    // corresponding Source.
    // (We need to use toMat and Keep.right since by default the materialized
    // value to the left is used)
    val runnableGraph: RunnableGraph[Source[String, NotUsed]] =
      producer.toMat(BroadcastHub.sink(bufferSize = 256))(Keep.right)

    // By running/materializing the producer, we get back a Source, which
    // gives us access to the elements published by the producer.
    val fromProducer: Source[String, NotUsed] = runnableGraph.run()

    // Print out messages from the producer in two independent consumers
    fromProducer.runForeach(msg => println("consumer1: " + msg))
    fromProducer.runForeach(msg => println("consumer2: " + msg))
  }

  def testPartialDownstreamDemands(): Unit = {
    /**
     * From the scaladoc comment for BroadcastHub.sink()
     *
     * bufferSize Buffer size used by the producer. Gives an upper bound on how "far" from each other two
                  concurrent consumers can be in terms of element. If this buffer is full, the producer
                   is backpressured. Must be a power of two and less than 4096.
     */
    val bufferSize = 4

    // BroadcastHub.sink(bufferSize = 4) will pull 4 elements immediately
    val producer = Source(1 to 10)
      .map(x => {println(s"upstream: ${x}");x})
      .toMat(BroadcastHub.sink(bufferSize))(Keep.right)
      .run()

    // 1st dynamic consumer
    val sinkPublisher1 = producer
      .map(x => {println(s"dynamic consumer 1: ${x}");x})
      .toMat(Sink.asPublisher(false))(Keep.right)
      .run()
    val sinkSubscriber1 = TestSubscriber.manualProbe[Int]()
    sinkPublisher1.subscribe(sinkSubscriber1)
    val sinkSubscription1 = sinkSubscriber1.expectSubscription()

    // 2nd dynamic consumer
    val sinkPublisher2 = producer
      .map(x => {println(s"dynamic consumer 2: ${x}");x})
      .toMat(Sink.asPublisher(false))(Keep.right)
      .run()
    val sinkSubscriber2 = TestSubscriber.manualProbe[Int]()
    sinkPublisher2.subscribe(sinkSubscriber2)
    val sinkSubscription2 = sinkSubscriber2.expectSubscription()

    /**
     * Here we pull elements from dynamic consumers
     */
    sinkSubscription1.request(1) //This pulls 1 from Broadcast Hub
    sinkSubscription2.request(1) //This pulls 1 from Broadcast Hub, for the consumer 2
    sinkSubscription1.request(3) //This pulls 2, 3, 4 from Broadcast Hub

    sinkSubscription1.request(6) //This doesn't pull anything as BroadcastHub's
  }

  /**
   * The difference from testPartialDownstreamDemands is that the
   */
  def testPartialDownstreamCancel(): Unit = {
    val bufferSize = 4

    // BroadcastHub.sink(bufferSize = 4) will pull 4 elements immediately
    val producer = Source(1 to 10)
      .map(x => {println(s"upstream: ${x}");x})
      .toMat(BroadcastHub.sink(bufferSize))(Keep.right)
      .run()

    // 1st dynamic consumer
    val sinkPublisher1 = producer
      .map(x => {println(s"dynamic consumer 1: ${x}");x})
      .toMat(Sink.asPublisher(false))(Keep.right)
      .run()
    val sinkSubscriber1 = TestSubscriber.manualProbe[Int]()
    sinkPublisher1.subscribe(sinkSubscriber1)
    val sinkSubscription1 = sinkSubscriber1.expectSubscription()

    // 2nd dynamic consumer
    val sinkPublisher2 = producer
      .map(x => {println(s"dynamic consumer 2: ${x}");x})
      .toMat(Sink.asPublisher(false))(Keep.right)
      .run()
    val sinkSubscriber2 = TestSubscriber.manualProbe[Int]()
    sinkPublisher2.subscribe(sinkSubscriber2)
    val sinkSubscription2 = sinkSubscriber2.expectSubscription()

    /**
     * Here we pull elements from dynamic consumers
     */
    sinkSubscription1.request(1) //This pulls 1 from Broadcast Hub
    sinkSubscription2.request(1) //This pulls 1 from Broadcast Hub, for the consumer 2
    sinkSubscription1.request(3) //This pulls 2, 3, 4 from Broadcast Hub
    sinkSubscription1.request(6) //This pulls all the remaining elements after sinkSubscription2.cancel()

    sinkSubscription2.cancel()

  }

  def testPartialDownstreamDemands2(): Unit = {
    try {
      val producer = Source(1 to 10).map(x => {println(s"upstream: ${x}");x}).toMat(BroadcastHub.sink(4))(Keep.right).run()

      val sinkSubscriber1 = TestSubscriber.manualProbe[Int]()
      producer.map(x => {println(s"dynamic consumer 1: ${x}");x}).to(Sink.fromSubscriber(sinkSubscriber1)).run()

      val sinkSubscriber2 = TestSubscriber.manualProbe[Int]()
      producer.map(x => {println(s"dynamic consumer 2: ${x}");x}).to(Sink.fromSubscriber(sinkSubscriber2)).run()

      sinkSubscriber1.expectNext()
      sinkSubscriber1.expectNext()
      sinkSubscriber1.expectNext()
      sinkSubscriber1.expectNext()
      sinkSubscriber1.expectNext() //this one throws, as it causes the BroadcastHub's buffer exceeding its bufferSize = 4
    }
    catch {
      case e:  AssertionError => println(s"expected assertion error: ${e}\n${e.getMessage}")
    }
  }

  def testCancelled(): Unit ={
    val (firstElem, source) = Source(1 to 3)
      .map(x => {println(s"upstream: ${x}");x})
      //.toMat(BroadcastHub.sink(1))(Keep.both)
      .toMat(Sink.fromGraph(new MyBroadcastHub[Int](1)))(Keep.both)
      .run()

    source.map(x => {println(s"dynamic consumer 1: ${x}");x}).runWith(Sink.seq)
    source.map(x => {println(s"dynamic consumer 2: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 3: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 4: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 5: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 6: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 7: ${x}");x}).runWith(Sink.seq)
//    source.map(x => {println(s"dynamic consumer 8: ${x}");x}).runWith(Sink.seq)

    // Ensure subscription of Sinks. This is racy but there is no event we can hook into here.
    Thread.sleep(100)
  }

  def testIssue23205(): Unit ={
    val (promise, source) = Source.maybe[Int].concat(Source(2 to 10))
      .map(x => {println(s"upstream: ${x}");x})
      .toMat(BroadcastHub.sink(1))(Keep.both).run()

    val f1 = source.map(x => {println(s"dynamic consumer 1: ${x}");x}).runWith(Sink.cancelled)
    val f2 = source.map(x => {println(s"dynamic consumer 2: ${x}");x}).runWith(Sink.seq)

    // Ensure subscription of Sinks. This is racy but there is no event we can hook into here.
    Thread.sleep(100)
    promise.success(Some(1))
    val result = Await.result(f2, 1 second)
    println(result)
  }


  def main(args: Array[String]): Unit = {
    try {
      //Wrapper("testPartialDownstreamDemands")(testPartialDownstreamDemands)
      //Wrapper("testPartialDownstreamDemands2")(testPartialDownstreamDemands2)
      //Wrapper("testPartialDownstreamCancel")(testPartialDownstreamCancel)
      Wrapper("testCancelled")(testCancelled)
      //Wrapper("testIssue23205")(testIssue23205)
      //Wrapper("test")(test)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
