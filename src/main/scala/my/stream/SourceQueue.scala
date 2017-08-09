package my.stream

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.TestSubscriber
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult}
import my.wrapper.Wrapper

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Success, Failure}

object SourceQueue {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def sourceQueue(): Unit = {
    val queue = Source.queue[Int](5, OverflowStrategy.backpressure).toMat(Sink.foreach(println(_)))(Keep.left).run()
    queue.offer(1)
    println(queue)
  }

  def sourceQueueComplete(): Unit = {
    val (queue, fut) = Source.queue[Int](5, OverflowStrategy.backpressure).toMat(Sink.foreach(println(_)))(Keep.both).run()
    queue.offer(1)
    queue.offer(2)
    queue.offer(3)
    queue.complete()
    println(Await.result(fut, 500 milliseconds))
  }

  def sourceQueueOverflow(bufferSize: Int, overflowStrategy: OverflowStrategy)(): Unit = {
    println(s"OverflowStrategy = ${overflowStrategy}")

    val ((sourceQueue, fut), sinkPublisher) = Source.queue[Int](bufferSize, overflowStrategy)
      .map(x => {println(s"processing: ${x}"); x})
      .watchTermination()(Keep.both)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(3)

    val sb = new StringBuilder()
    for(i <- 1 to 20){
      val f = sourceQueue.offer(i)
      import scala.concurrent.ExecutionContext.Implicits.global
      f onComplete{
        case Success(x) => sb.append(s"SourceQueue.offer(${i}) returned ${x}\n")
        case Failure(x) => sb.append(s"SourceQueue.offer(${i}) failed with ${x}\n")
      }
    }
    sinkSubscription.request(30)
    sourceQueue.complete()
    println(Await.result(fut, 500 milliseconds))
    println(sb.toString())
  }

  /**
   * QueueSourceSpec -> "fail offer future when stream is completed"
   */
  def sourceQueueFuture(): Unit ={
    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    // Source.to is Keep.left
    val queue = Source.queue(1, OverflowStrategy.dropNew).to(Sink.fromSubscriber(sinkSubscriber)).run()

    val subscription = sinkSubscriber.expectSubscription

    import scala.concurrent.ExecutionContext.Implicits.global
    val fut = queue.offer(1)
    fut.onComplete{
      case Success(x) => println(s"success: ${x}") //success: Enqueued
      case Failure(x) => println(s"failure: ${x}")
    }

    val fut2 = queue.offer(1)
    fut2.onComplete{
      case Success(x) => println(s"success: ${x}") //success: Dropped
      case Failure(x) => println(s"failure: ${x}")
    }

    subscription.cancel()
    Thread.sleep(500)

    queue.offer(1).failed.foreach{
      e => println(s"${e}")
      //failure: java.lang.IllegalStateException: Stream is terminated. SourceQueue is detached
    }

    val fut3 = queue.offer(1)
    fut3.onComplete{
      case Success(x) => println(s"success: ${x}") //success: Dropped
      case Failure(x) => println(s"failure: ${x}")
    }
    //This future does not complete
  }

  def main(args: Array[String]): Unit = {
    try{
      Wrapper("sourceQueue")(sourceQueue)
      Wrapper("sourceQueueComplete")(sourceQueueComplete)
      /**
       * From the API comment:
       *   The strategy [[akka.stream.OverflowStrategy.backpressure]] will not complete last `offer():Future`
       *   call when buffer is full.
       */
      Wrapper("sourceQueueOverflow")(sourceQueueOverflow(5, OverflowStrategy.backpressure))
      Wrapper("sourceQueueOverflow")(sourceQueueOverflow(5, OverflowStrategy.dropTail))
      Wrapper("sourceQueueOverflow")(sourceQueueOverflow(5, OverflowStrategy.dropHead))
      Wrapper("sourceQueueOverflow")(sourceQueueOverflow(5, OverflowStrategy.dropNew))
      Wrapper("sourceQueueFuture")(sourceQueueFuture)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
