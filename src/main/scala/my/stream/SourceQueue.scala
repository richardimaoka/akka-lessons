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
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
