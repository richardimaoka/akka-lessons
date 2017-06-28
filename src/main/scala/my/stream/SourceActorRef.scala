package my.stream

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Keep, Sink, Source}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

object SourceActorRef {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def sourceActorRefTest(bufferSize: Int, overflowStrategy: OverflowStrategy)(): Unit ={
    val (ref, queue) = Source.actorRef[Int](bufferSize, overflowStrategy)
      .toMat(Sink.queue())(Keep.both)
      .run()

    for(i <- 1 to 10) {
      ref ! i
    }

    for(i <- 1 to 5) {
      val fut = queue.pull()

      val result = Await.result(fut, 100 milliseconds)
      println(s"pulled result = ${result}")
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("sourceActorRefTest")(sourceActorRefTest(5, OverflowStrategy.dropHead))
      Wrapper("sourceActorRefTest")(sourceActorRefTest(5, OverflowStrategy.dropTail))
      Wrapper("sourceActorRefTest")(sourceActorRefTest(5, OverflowStrategy.dropNew))
      Wrapper("sourceActorRefTest")(sourceActorRefTest(5, OverflowStrategy.dropBuffer))
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
