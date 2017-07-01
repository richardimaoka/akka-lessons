package my.stream

import java.io.{IOException, OutputStream}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeoutException}

import akka.actor.ActorSystem
import akka.stream.ActorAttributes.Dispatcher
import akka.stream.Attributes._
import akka.stream._
import akka.stream.scaladsl.{Keep, Sink, Source, StreamConverters}
import akka.stream.stage._
import akka.stream.testkit.scaladsl.TestSink
import akka.stream.testkit.{GraphStageMessages, TestSourceStage}
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import my.stream.OutputStreamSourceStage._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration.Duration._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

/**
 * GraphStageLogic will extend it
 * Interesting ... it is not contained as a member
 */
trait CallbackWrapper[T] extends AsyncCallback[T] {
  private trait CallbackState
  private case class NotInitialized(list: List[T]) extends CallbackState
  private case class Initialized(f: T ⇒ Unit) extends CallbackState
  private case class Stopped(f: T ⇒ Unit) extends CallbackState

  /*
   * To preserve message order when switching between not initialized / initialized states
   * lock is used. Case is similar to RepointableActorRef
   */
  private[this] final val lock = new ReentrantLock

  private[this] val callbackState = new AtomicReference[CallbackState](NotInitialized(Nil))

  def stopCallback(f: T ⇒ Unit): Unit = locked {
    callbackState.set(Stopped(f))
  }

  def initCallback(f: T ⇒ Unit): Unit = locked {
    val list = (callbackState.getAndSet(Initialized(f)): @unchecked) match {
      case NotInitialized(l) ⇒ l
    }
    list.reverse.foreach(f)
  }

  override def invoke(arg: T): Unit = locked {
    callbackState.get() match {
      case Initialized(cb)          ⇒ cb(arg)
      case list @ NotInitialized(l) ⇒ callbackState.compareAndSet(list, NotInitialized(arg :: l))
      case Stopped(cb)              ⇒ cb(arg)
    }
  }

  private[this] def locked(body: ⇒ Unit): Unit = {
    lock.lock()
    try body finally lock.unlock()
  }
}

object ActorMaterializerHelper {
  /**
   * INTERNAL API
   */
  def downcast(materializer: Materializer): ActorMaterializer =
    materializer match { //FIXME this method is going to cause trouble for other Materializer implementations
      case m: ActorMaterializer ⇒ m
      case _ ⇒ throw new IllegalArgumentException(s"required [${classOf[ActorMaterializer].getName}] " +
        s"but got [${materializer.getClass.getName}]")
    }
}

object OutputStreamSourceStage {
  sealed trait AdapterToStageMessage
  case object Flush extends AdapterToStageMessage
  case object Close extends AdapterToStageMessage

  sealed trait DownstreamStatus
  case object Ok extends DownstreamStatus
  case object Canceled extends DownstreamStatus

  val IODispatcher = ActorAttributes.IODispatcher

  def asOutputStream(writeTimeout: FiniteDuration = 5.seconds): Source[ByteString, OutputStream] =
    Source.fromGraph(new OutputStreamSourceStage(writeTimeout))
}

/**
 * Creates a Source which when materialized will return an [[OutputStream]] which it is possible
 * to write the ByteStrings to the stream this Source is attached to.
 *
 * This Source is intended for inter-operation with legacy APIs since it is INHERENTLY BLOCKING.
 *    -> *** what this means is ... it uses val dataQueue = new LinkedBlockingQueue[ByteString](maxBuffer) for blocking ***
 *    -> ******and METHODS ON THE MATERIALIZED OutputStream WILL BLOCK ******
 *
 * You can configure the default dispatcher for this Source by changing the `akka.stream.blocking-io-dispatcher` or
 * set it for a given Source by using [[akka.stream.ActorAttributes]].
 *
 * The created [[OutputStream]] will be closed when the [[Source]] is cancelled, and closing the [[OutputStream]]
 * will complete this [[Source]].
 *
 * @param writeTimeout the max time the write operation on the materialized OutputStream should block, defaults to 5 seconds
 */
class OutputStreamSourceStage(writeTimeout: FiniteDuration) extends GraphStageWithMaterializedValue[SourceShape[ByteString], OutputStream] {
  val out = Outlet[ByteString]("OutputStreamSource.out")
  override def initialAttributes: Attributes = Attributes.name("OutputStreamSources") and ActorAttributes.Dispatcher("akka.stream.default-blocking-io-dispatcher")
  override val shape: SourceShape[ByteString] = SourceShape.of(out)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, OutputStream) = {
    // This is just the buffer size, which is used by the actual dataQueue later
    val maxBuffer = inheritedAttributes.getAttribute(classOf[InputBuffer], InputBuffer(16, 16)).max

    val dispatcherId = inheritedAttributes.get[Dispatcher](IODispatcher).dispatcher

    require(maxBuffer > 0, "Buffer size must be greater than 0")

    /**
     * Ahh see how OutputStreamAdapter's dataQueue: BlockingQueue[ByteString] uses it to
     * block the materialized Java OutputStream.write() method
     *
     * dataQueue.put() blocks if this exact Queue is full
     */
    val dataQueue = new LinkedBlockingQueue[ByteString](maxBuffer)
    val downstreamStatus = new AtomicReference[DownstreamStatus](Ok)

    final class OutputStreamSourceLogic extends GraphStageLogic(shape)
      with CallbackWrapper[(AdapterToStageMessage, Promise[Unit])] {

      var flush: Option[Promise[Unit]] = None
      var close: Option[Promise[Unit]] = None

      private var dispatcher: ExecutionContext = null // set in preStart
      /**
       *  This blockingThread is used only in
       *   1. onPull() where Future's body assigns currentThread to blockingThread,
       *      (i.e.) blockingThread indicates
       */
      private var blockingThread: Thread = null // for postStop interrupt

      // getAsyncCallback returns an AsyncCallback instance,
      // which allows external threads call the body of getAsyncCallback{} via AsyncCallback.invoke()
      private val upstreamCallback: AsyncCallback[(AdapterToStageMessage, Promise[Unit])] =
        //(AdapterToStageMessage, Promise[Unit]) is the parameter type of AsyncCallback
        getAsyncCallback(onAsyncMessage)

      private def onAsyncMessage(event: (AdapterToStageMessage, Promise[Unit])): Unit =
        event._1 match {
          case Flush ⇒
            flush = Some(event._2)
            sendResponseIfNeed()
          case Close ⇒
            close = Some(event._2)
            sendResponseIfNeed()
        }

      /**
       * Used in OutputStreamAdapter's sendToStage(), which is called from flush() and close()
       */
      def wakeUp(msg: AdapterToStageMessage): Future[Unit] = {
        val p = Promise[Unit]()
        this.invoke((msg, p))
        p.future
      }

      private def unblockUpstream(): Boolean =
        flush match {
          case Some(p) ⇒
            p.complete(Success(()))
            flush = None
            true
          case None ⇒ close match {
            case Some(p) ⇒
              downstreamStatus.set(Canceled)
              p.complete(Success(()))
              close = None
              completeStage()
              true
            case None ⇒ false
          }
        }

      private def sendResponseIfNeed(): Unit =
        if (downstreamStatus.get() == Canceled || dataQueue.isEmpty) unblockUpstream()

      private def onPush(data: ByteString): Unit =
        if (downstreamStatus.get() == Ok) {
          push(out, data)
          sendResponseIfNeed()
        }

      override def preStart(): Unit = {
        //val dispatcherId = inheritedAttributes.get[Dispatcher](IODispatcher).dispatcher
        println(s"initializing dispatcher = ${dispatcherId}, from initial attributes")

        dispatcher = ActorMaterializerHelper.downcast(materializer).system.dispatchers.lookup(dispatcherId)
        super.preStart()
        initCallback(upstreamCallback.invoke)
      }

      setHandler(out, new OutHandler {
        override def onPull(): Unit = {
          implicit val ec = dispatcher
          Future {
            // keep track of the thread for postStop interrupt
            blockingThread = Thread.currentThread()
            println(s"blockingThread is assigned to ${blockingThread} in onPull()")
            try {
              /**
               * THIS IS A BLOCKING CALL !!!
               * (i.e.) next onPull() waits
               */
              dataQueue.take()
            } catch {
              case _: InterruptedException ⇒
                Thread.interrupted()
                println(s"interrupted!!!!!!!!!!!!")
                ByteString.empty
            } finally {
              blockingThread = null
              println(s"blockingThread is assigned to null in onPull()")
            }
          }.onComplete( (t: Try[ByteString]) => {
            println(s"calling downstreamCallback on ${t}")
            /**
             * downstreamCallback.invoke() inside Future, (i.e.) from an external thread
             */
            downstreamCallback.invoke(t) //downstreamCallback is defined below
          })
        }
      })
      // getAsyncCallback returns an AsyncCallback instance,
      // which allows external threads call the body of getAsyncCallback{} via AsyncCallback.invoke()
      private val downstreamCallback: AsyncCallback[Try[ByteString]] = //Try[ByteString] is the parameter type of AsyncCallback
        getAsyncCallback {
          case Success(elem) ⇒ onPush(elem)
          case Failure(ex)   ⇒ failStage(ex)
        }

      override def postStop(): Unit = {
        //assuming there can be no further in messages
        println("set downstream status to Canceled")
        downstreamStatus.set(Canceled)
        dataQueue.clear()
        // if blocked reading, make sure the take() completes
        dataQueue.put(ByteString.empty)
        // interrupt any pending blocking take
        if (blockingThread != null){
          println("interrupting blockingThread from postStop()")
          blockingThread.interrupt()
        }
        super.postStop()
      }
    }

    val logic = new OutputStreamSourceLogic
    (logic, new OutputStreamAdapter(dataQueue, downstreamStatus, logic.wakeUp, writeTimeout))
  }
}

/**
 * Implementation which bridges between OutputStreamStage and Java's OutputStream
 * extends OutputStream !!
 */
class OutputStreamAdapter(
  dataQueue:        BlockingQueue[ByteString],
  downstreamStatus: AtomicReference[DownstreamStatus],
  sendToStage:      (AdapterToStageMessage) ⇒ Future[Unit], //OutputStreamStage's wakeUp()
  writeTimeout:     FiniteDuration) extends OutputStream {

  var isActive = true
  var isPublisherAlive = true
  val publisherClosedException = new IOException("Reactive stream is terminated, no writes are possible")

  @scala.throws(classOf[IOException])
  private[this] def send(sendAction: () ⇒ Unit): Unit = {
    if (isActive) {
      if (isPublisherAlive) sendAction()
      else throw publisherClosedException
    } else throw new IOException("OutputStream is closed")
  }

  @scala.throws(classOf[IOException])
  private[this] def sendData(data: ByteString): Unit = {
    println(s"sendData ${data}")
    send(() ⇒ {
      try {
        dataQueue.put(data)
      } catch {
        case NonFatal(ex) ⇒ throw new IOException(ex)
      }
      if (downstreamStatus.get() == Canceled) {
        isPublisherAlive = false
        throw publisherClosedException
      }
    })
    println(s"sendData finished")
  }


  @scala.throws(classOf[IOException])
  private[this] def sendMessage(message: AdapterToStageMessage, handleCancelled: Boolean = true) =
    send(() ⇒
      try {
        Await.ready(sendToStage(message), writeTimeout)
        if (downstreamStatus.get() == Canceled && handleCancelled) {
          //Publisher considered to be terminated at earliest convenience to minimize messages sending back and forth
          isPublisherAlive = false
          throw publisherClosedException
        }
      } catch {
        case e: IOException ⇒ throw e
        case NonFatal(e)    ⇒ throw new IOException(e)
      })

  @scala.throws(classOf[IOException])
  override def write(b: Int): Unit = {
    sendData(ByteString(b))
  }

  @scala.throws(classOf[IOException])
  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    sendData(ByteString.fromArray(b, off, len))
  }

  @scala.throws(classOf[IOException])
  override def flush(): Unit = sendMessage(Flush)

  @scala.throws(classOf[IOException])
  override def close(): Unit = {
    sendMessage(Close, handleCancelled = false)
    isActive = false
  }
}

class OutputStreamSourceSpec extends TestKit(ActorSystem("OutputStreamSourceSpec")) with WordSpecLike with Matchers with BeforeAndAfterAll {
  import system.dispatcher

  val UnboundedMailboxConfig = ConfigFactory.parseString("""akka.actor.default-mailbox.mailbox-type = "akka.dispatch.UnboundedMailbox"""")

  val settings = ActorMaterializerSettings(system).withDispatcher("akka.actor.default-dispatcher")
  implicit val materializer = ActorMaterializer(settings)

  val timeout = 1.seconds
  val bytesArray = Array.fill[Byte](3)(Random.nextInt(1024).asInstanceOf[Byte])
  val byteString = ByteString(bytesArray)

  def expectTimeout[T](f: Future[T], timeout: Duration) =
    the[Exception] thrownBy Await.result(f, timeout) shouldBe a[TimeoutException]

  def expectSuccess[T](f: Future[T], value: T) =
    Await.result(f, remainingOrDefault) should be(value)

  def assertNoBlockedThreads(): Unit = {
    def threadsBlocked =
      ManagementFactory.getThreadMXBean.dumpAllThreads(true, true).toSeq
        .filter(t ⇒ t.getThreadName.startsWith("OutputStreamSourceSpec") &&
          t.getLockName != null &&
          t.getLockName.startsWith("java.util.concurrent.locks.AbstractQueuedSynchronizer") &&
          t.getStackTrace.exists(s ⇒ s.getClassName.startsWith(classOf[OutputStreamSourceStage].getName)))

    awaitAssert(threadsBlocked should ===(Seq()), 5.seconds, interval = 500.millis)
  }

  "OutputStreamSource" must {
    "read bytes from OutputStream" in {
      println("\nread bytes from OutputStream -------------------------------------")
      /**
       * Equivalent to StreamConverters.asOutputStream(): Source[ByteString, OutputStream] //Spits out BytString, and materializes to Java's OutputStream
       */
      val (outputStream, probe) = OutputStreamSourceStage.asOutputStream()
      /**
       * Creates a Source which when materialized will return an [[OutputStream]] which it is possible
       * to write the ByteStrings to the stream this Source is attached to.
       * This Source is intended for inter-operation with legacy APIs since it is inherently blocking.
       */
        .toMat(TestSink.probe[ByteString])(Keep.both)
        .run()

      val s = probe.expectSubscription()

      // Java's OutputStream.write() method, a blocking call
      //   public void write(byte b[]) throws IOException
      outputStream.write(bytesArray) // val bytesArray = Array.fill[Byte](3)(Random.nextInt(1024).asInstanceOf[Byte])
      s.request(1)
      probe.expectNext(byteString)   // val byteString = ByteString(bytesArray)

      outputStream.close()
      probe.expectComplete()
    }

    "block writes when buffer is full" in {
      println("\nblock writes when buffer is full ----------------------------------")
      //Same as the above test case
      val (outputStream, probe) = OutputStreamSourceStage.asOutputStream().toMat(TestSink.probe[ByteString])(Keep.both)
        //This part is new, different from the above test case
        .withAttributes(Attributes.inputBuffer(16, 16)).run

      val s = probe.expectSubscription()

      (1 to 16).foreach { _ ⇒ outputStream.write(bytesArray) }

      // blocking call in Future, Java's outputStream.write
      // using ExecutionContext = import system.dispatcher
      val f = Future(outputStream.write(bytesArray))

      // expectTimeout because buffer is full ..
      // so Java's OutputStream is "somehow" blocked by OutputStreamSourceStage internals?
      println("wait for future which is expected to timeout")
      expectTimeout(f, timeout) // 3.seconds
      probe.expectNoMsg(Zero)
      Thread.sleep(500)

      s.request(17)
      expectSuccess(f, ())
      probe.expectNextN(List.fill(17)(byteString).toSeq)

      outputStream.close()
      probe.expectComplete()
    }

    "throw error when write after stream is closed" in {
      println("\nthrow error when write after stream is closed ----------------------------------")
      val (outputStream, probe) = OutputStreamSourceStage.asOutputStream().toMat(TestSink.probe[ByteString])(Keep.both).run

      probe.expectSubscription()
      outputStream.close()
      probe.expectComplete()

      the[Exception] thrownBy outputStream.write(bytesArray) shouldBe a[IOException]
      /**
       * [info]   java.io.IOException: OutputStream is closed
       * [info]   at akka.stream.impl.io.OutputStreamAdapter.send(OutputStreamSourceStage.scala:171)
       * [info]   at akka.stream.impl.io.OutputStreamAdapter.sendData(OutputStreamSourceStage.scala:176)
       * [info]   at akka.stream.impl.io.OutputStreamAdapter.write(OutputStreamSourceStage.scala:208)
       * [info]   at java.io.OutputStream.write(OutputStream.java:75)
       * [info]   at my.stream.OutputStreamSourceSpec.$anonfun$new$7(OutputStreamSourceSpec.scala:361)
       * [info]   at scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:12)
       * [info]   at org.scalatest.OutcomeOf.outcomeOf(OutcomeOf.scala:85)
       * [info]   at org.scalatest.OutcomeOf.outcomeOf$(OutcomeOf.scala:83)
       * [info]   at org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
       * [info]   at org.scalatest.Transformer.apply(Transformer.scala:22)
       */
    }

    "throw IOException when writing to the stream after the subscriber has cancelled the reactive stream" in {
      println("\nthrow IOException when writing to the stream after the subscriber has cancelled the reactive stream ----------------------------------")
      val sourceProbe = TestProbe()
      val (outputStream, probe) = TestSourceStage(new OutputStreamSourceStage(timeout), sourceProbe)
        .toMat(TestSink.probe[ByteString])(Keep.both).run

      val s = probe.expectSubscription()

      outputStream.write(bytesArray)
      s.request(1)
      sourceProbe.expectMsg(GraphStageMessages.Pull)

      probe.expectNext(byteString)

      s.cancel()
      sourceProbe.expectMsg(GraphStageMessages.DownstreamFinish)
      the[Exception] thrownBy outputStream.write(bytesArray) shouldBe a[IOException]
    }

    "fail to materialize with zero sized input buffer" in {
      an[IllegalArgumentException] shouldBe thrownBy {
        StreamConverters.asOutputStream(timeout)
          .withAttributes(inputBuffer(0, 0))
          .runWith(Sink.head)
        /*
             With Sink.head we test the code path in which the source
             itself throws an exception when being materialized. If
             Sink.ignore is used, the same exception is thrown by
             Materializer.
             */
      }
    }

    "not leave blocked threads" in {
      // make sure previous tests didn't leak
      assertNoBlockedThreads()

      val (outputStream, probe) = StreamConverters.asOutputStream(timeout)
        .toMat(TestSink.probe[ByteString])(Keep.both).run()(materializer)

      val sub = probe.expectSubscription()

      // triggers a blocking read on the queue
      // and then cancel the stage before we got anything
      sub.request(1)
      sub.cancel()

      assertNoBlockedThreads()
    }

    "not leave blocked threads when materializer shutdown" in {
      val materializer2 = ActorMaterializer(settings)
      val (outputStream, probe) = StreamConverters.asOutputStream(timeout)
        .toMat(TestSink.probe[ByteString])(Keep.both).run()(materializer2)

      val sub = probe.expectSubscription()

      // triggers a blocking read on the queue
      // and then shutdown the materializer before we got anything
      sub.request(1)
      materializer2.shutdown()

      assertNoBlockedThreads()
    }

    "correctly complete the stage after close" in {
      // actually this was a race, so it only happened in at least one of 20 runs

      val bufSize = 4
      val sourceProbe = TestProbe()
      val (outputStream, probe) = OutputStreamSourceStage.asOutputStream(timeout)
        .addAttributes(Attributes.inputBuffer(bufSize, bufSize))
        .toMat(TestSink.probe[ByteString])(Keep.both).run

      // fill the buffer up
      (1 to (bufSize - 1)).foreach(outputStream.write)
      Future {
        outputStream.close()
      }
      // here is the race, has the elements reached the stage buffer yet?
      Thread.sleep(500)
      probe.request(bufSize - 1)
      probe.expectNextN(bufSize - 1)
      probe.expectComplete()
    }
  }

  "see what happens wiht multiple downstream demands without upstream send()" in {
    println("\nsee what happens wiht multiple downstream demands without upstream send() ----------------------------------")
    //Same as the above test case
    val (outputStream, probe) = OutputStreamSourceStage.asOutputStream().toMat(TestSink.probe[ByteString])(Keep.both)
      //This part is new, different from the above test case
      .withAttributes(Attributes.inputBuffer(16, 16)).run

    val s = probe.expectSubscription()
    outputStream.write(bytesArray)
    s.request(17)
    probe.expectNext(byteString)   // val byteString = ByteString(bytesArray)

    outputStream.close()

    /**
     * As postStop() will interrupt the blocking thread (used in onPull()'s Future) within the graph stage
     * an exception like following due to thread interruption is expected
     *
     * [ERROR] [07/02/2017 00:55:35.821] [OutputStreamSourceSpec-akka.stream.default-blocking-io-dispatcher-20] [akka://OutputStreamSourceSpec/user/StreamSupervisor-0/flow-8-2-OutputStreamSources] Error during postStop in [my.stream.OutputStreamSourceStage@5e749f17]: null
        java.lang.NullPointerException
                at my.stream.OutputStreamSourceStage$OutputStreamSourceLogic$1.postStop(OutputStreamSourceSpec.scala:257)
                at akka.stream.impl.fusing.GraphInterpreter.finalizeStage(GraphInterpreter.scala:551)
                at akka.stream.impl.fusing.GraphInterpreter.afterStageHasRun(GraphInterpreter.scala:532)
                at akka.stream.impl.fusing.GraphInterpreter.runAsyncInput(GraphInterpreter.scala:447)
                at akka.stream.impl.fusing.GraphInterpreterShell$AsyncInput.execute(ActorGraphInterpreter.scala:453)
                at akka.stream.impl.fusing.GraphInterpreterShell.processEvent(ActorGraphInterpreter.scala:546)
                at akka.stream.impl.fusing.ActorGraphInterpreter.akka$stream$impl$fusing$ActorGraphInterpreter$$processEvent(ActorGraphInterpreter.scala:725)
                at akka.stream.impl.fusing.ActorGraphInterpreter$$anonfun$receive$1.applyOrElse(ActorGraphInterpreter.scala:740)
                at akka.actor.Actor.aroundReceive(Actor.scala:513)
                at akka.actor.Actor.aroundReceive$(Actor.scala:511)
                at akka.stream.impl.fusing.ActorGraphInterpreter.aroundReceive(ActorGraphInterpreter.scala:650)
                at akka.actor.ActorCell.receiveMessage(ActorCell.scala:527)
                at akka.actor.ActorCell.invoke(ActorCell.scala:496)
                at akka.dispatch.Mailbox.processMailbox(Mailbox.scala:257)
                at akka.dispatch.Mailbox.run(Mailbox.scala:224)
                at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
                at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
                at java.lang.Thread.run(Thread.java:745)
     */

    probe.expectComplete()
  }
}
