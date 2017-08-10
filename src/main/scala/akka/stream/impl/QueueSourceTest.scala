package akka.stream.impl

import akka.Done
import akka.actor.ActorSystem
import akka.annotation.InternalApi
import akka.stream.OverflowStrategies.{Backpressure, _}
import akka.stream.scaladsl.{Sink, Source, SourceQueueWithComplete}
import akka.stream.stage._
import akka.stream.testkit.TestSubscriber
import akka.stream.{BufferOverflowException, QueueOfferResult, _}
import my.wrapper.Wrap

import scala.concurrent.{Future, Promise}
/**
 * INTERNAL API
 */
@InternalApi private[akka] object MyQueueSource {
  sealed trait Input[+T]
  final case class Offer[+T](elem: T, promise: Promise[QueueOfferResult]) extends Input[T]
  case object Completion extends Input[Nothing]
  final case class Failure(ex: Throwable) extends Input[Nothing]
}

@InternalApi private[akka] final class MyQueueSource[T](maxBuffer: Int, overflowStrategy: OverflowStrategy) extends GraphStageWithMaterializedValue[SourceShape[T], SourceQueueWithComplete[T]] {
  import MyQueueSource._

  val out = Outlet[T]("myQueueSource.out")
  override val shape: SourceShape[T] = SourceShape.of(out)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes) = {
    val completion = Promise[Done]
    val stageLogic = new GraphStageLogic(shape) with CallbackWrapper[Input[T]] with OutHandler {
      var buffer: Buffer[T] = _
      var pendingOffer: Option[Offer[T]] = None
      var terminating = false

      override def preStart(): Unit = {
        println("MyQueueSource preStart(): called")
        if (maxBuffer > 0) buffer = Buffer(maxBuffer, materializer)
        initCallback(callback.invoke)
      }
      override def postStop(): Unit = {
        val exception = new AbruptStageTerminationException(this)
        completion.tryFailure(exception)
        stopCallback {
          case Offer(elem, promise) ⇒ {
            println(s"MyQueueSource postStop(${elem}): completing Promise with ${exception}")
            promise.failure(exception)
          }
          case _                    ⇒ // ignore
        }
      }

      private def enqueueAndSuccess(offer: Offer[T]): Unit = {
        buffer.enqueue(offer.elem)
        offer.promise.success(QueueOfferResult.Enqueued)
      }

      private def bufferElem(offer: Offer[T]): Unit = {
        if (!buffer.isFull) {
          enqueueAndSuccess(offer)
        } else overflowStrategy match {
          case DropHead ⇒
            buffer.dropHead()
            enqueueAndSuccess(offer)
          case DropTail ⇒
            buffer.dropTail()
            enqueueAndSuccess(offer)
          case DropBuffer ⇒
            buffer.clear()
            enqueueAndSuccess(offer)
          case DropNew ⇒
            offer.promise.success(QueueOfferResult.Dropped)
          case Fail ⇒
            val bufferOverflowException = new BufferOverflowException(s"Buffer overflow (max capacity was: $maxBuffer)!")
            offer.promise.success(QueueOfferResult.Failure(bufferOverflowException))
            completion.failure(bufferOverflowException)
            failStage(bufferOverflowException)
          case Backpressure ⇒
            pendingOffer match {
              case Some(_) ⇒
                offer.promise.failure(new IllegalStateException("You have to wait for previous offer to be resolved to send another request"))
              case None ⇒
                pendingOffer = Some(offer)
            }
        }
      }

      private val callback: AsyncCallback[Input[T]] = getAsyncCallback {

        case offer @ Offer(elem, promise) ⇒
          if (maxBuffer != 0) {
            bufferElem(offer)
            if (isAvailable(out)) push(out, buffer.dequeue())
          } else if (isAvailable(out)) {
            push(out, elem)
            promise.success(QueueOfferResult.Enqueued)
          } else if (pendingOffer.isEmpty)
            pendingOffer = Some(offer)
          else overflowStrategy match {
            case DropHead | DropBuffer ⇒
              pendingOffer.get.promise.success(QueueOfferResult.Dropped)
              pendingOffer = Some(offer)
            case DropTail | DropNew ⇒
              promise.success(QueueOfferResult.Dropped)
            case Fail ⇒
              val bufferOverflowException = new BufferOverflowException(s"Buffer overflow (max capacity was: $maxBuffer)!")
              promise.success(QueueOfferResult.Failure(bufferOverflowException))
              completion.failure(bufferOverflowException)
              failStage(bufferOverflowException)
            case Backpressure ⇒
              promise.failure(new IllegalStateException("You have to wait for previous offer to be resolved to send another request"))
          }

        case Completion ⇒
          if (maxBuffer != 0 && buffer.nonEmpty || pendingOffer.nonEmpty) terminating = true
          else {
            completion.success(Done)
            completeStage()
          }

        case Failure(ex) ⇒
          completion.failure(ex)
          failStage(ex)
      }

      setHandler(out, this)

      override def onDownstreamFinish(): Unit = {
        pendingOffer match {
          case Some(Offer(elem, promise)) ⇒
            promise.success(QueueOfferResult.QueueClosed)
            pendingOffer = None
          case None ⇒ // do nothing
        }
        completion.success(Done)
        completeStage()
      }

      override def onPull(): Unit = {
        if (maxBuffer == 0) {
          pendingOffer match {
            case Some(Offer(elem, promise)) ⇒
              push(out, elem)
              promise.success(QueueOfferResult.Enqueued)
              pendingOffer = None
              if (terminating) {
                completion.success(Done)
                completeStage()
              }
            case None ⇒
          }
        } else if (buffer.nonEmpty) {
          push(out, buffer.dequeue())
          pendingOffer match {
            case Some(offer) ⇒
              enqueueAndSuccess(offer)
              pendingOffer = None
            case None ⇒ //do nothing
          }
          if (terminating && buffer.isEmpty) {
            completion.success(Done)
            completeStage()
          }
        }
      }
    }

    (stageLogic, new SourceQueueWithComplete[T] {
      override def watchCompletion() = completion.future
      override def offer(element: T): Future[QueueOfferResult] = {
        val p = Promise[QueueOfferResult]
        stageLogic.invoke(Offer(element, p))
        p.future
      }
      override def complete(): Unit = {
        stageLogic.invoke(Completion)
      }
      override def fail(ex: Throwable): Unit = {
        stageLogic.invoke(Failure(ex))
      }
    })
  }
}

object QueueSourceTest {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def futureFailedTest(): Unit ={
    import scala.concurrent.ExecutionContext.Implicits.global

    val fut = Future(10)

    import scala.util.{Failure, Success}
    fut.failed.onComplete{
      case Success(x) => println(s"success: ${x}")
      case Failure(x) => println(s"failure: ${x}")
      //failure: java.util.NoSuchElementException: Future.failed not completed with a throwable.
    }
  }

  def sourceQueueFuture1(): Unit ={
    import scala.util.{Failure, Success}
    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    // Source.to is Keep.left
    val queue = Source.queue(1, OverflowStrategy.dropNew).to(Sink.fromSubscriber(sinkSubscriber)).run()

    val subscription = sinkSubscriber.expectSubscription

    import scala.concurrent.ExecutionContext.Implicits.global
    val fut = queue.offer(1)
    fut.onComplete{
      case Success(x) => println(s"sourceQueueFuture1 fut success: ${x}") //success: Enqueued
      case Failure(x) => println(s"sourceQueueFuture1 fut failure: ${x}")
    }

    val fut2 = queue.offer(1)
    fut2.onComplete{
      case Success(x) => println(s"sourceQueueFuture1 fut2 success: ${x}") //success: Dropped
      case Failure(x) => println(s"sourceQueueFuture1 fut2 failure: ${x}")
    }

    val futComp = queue.watchCompletion()
    futComp.onComplete{
      case Success(x) => println(s"sourceQueueFuture1 futComp success: ${x}") //success: Dropped
      case Failure(x) => println(s"sourceQueueFuture1 futComp failure: ${x}")
    }

    println("cancelling subscription")
    subscription.cancel()
    Thread.sleep(500)

    queue.offer(1).failed.foreach{
      e => println(s"${e}")
      //failure: java.lang.IllegalStateException: Stream is terminated. SourceQueue is detached
    }

    val fut3 = queue.offer(1)
    fut3.onComplete{
      case Success(x) => println(s"sourceQueueFuture1 fut3 success: ${x}") //success: Dropped
      case Failure(x) => println(s"sourceQueueFuture1 fut3 failure: ${x}")
    }
    //This future does not complete
  }

  /**
   * QueueSourceSpec -> "fail offer future when stream is completed"
   */
  def sourceQueueFuture2(): Unit ={
    import scala.util.{Failure, Success}

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()

    //Source.queue(1, OverflowStrategy.dropNew).to(Sink.fromSubscriber(s)).run()
    val queue = Source
      .fromGraph(new MyQueueSource[Int](maxBuffer = 1, overflowStrategy = OverflowStrategy.dropNew))
      .withAttributes(Attributes.name("myQueueSource"))
      // Source.to is Keep.left
      .to(Sink.fromSubscriber(sinkSubscriber))
      .run()

    val subscription = sinkSubscriber.expectSubscription

    import scala.concurrent.ExecutionContext.Implicits.global
    val fut = queue.offer(1)
    fut.onComplete{
      case Success(x) => println(s"sourceQueueFuture2 fut success: ${x}") //success: Enqueued
      case Failure(x) => println(s"sourceQueueFuture2 fut failure: ${x}")
    }

    val fut2 = queue.offer(2)
    fut2.onComplete{
      case Success(x) => println(s"sourceQueueFuture2 fut2 success: ${x}") //success: Dropped
      case Failure(x) => println(s"sourceQueueFuture2 fut2 failure: ${x}")
    }

    println("cancelling subscription")
    subscription.cancel()
    Thread.sleep(500)

    queue.offer(10).failed.foreach{
      e => println(s"${e}")
      //akka.stream.AbruptStageTerminationException:
      // GraphStage [akka.stream.impl.MyQueueSource$$anon$1@3abb1c3b] terminated abruptly, caused by for example materializer or actor system termination.
    }

    val fut3 = queue.offer(3)
    fut3.onComplete{
      case Success(x) => println(s"sourceQueueFuture2 fut3 success: ${x}")
      case Failure(x) => println(s"sourceQueueFuture2 fut3 failure: ${x}")
      //failure: akka.stream.AbruptStageTerminationException: GraphStage [akka.stream.impl.MyQueueSource$$anon$1@3abb1c3b] terminated abruptly, caused by for example materializer or actor system termination.
    }
    //This future does not complete
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrap("futureFailedTest")(futureFailedTest)
      Wrap("sourceQueueFuture1")(sourceQueueFuture1)
      Wrap("sourceQueueFuture2")(sourceQueueFuture2)
    } finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
