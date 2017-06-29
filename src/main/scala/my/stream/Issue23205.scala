package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{BroadcastHub, Keep, Sink, Source}

import scala.concurrent.duration._
import my.wrapper.Wrapper

import scala.concurrent.Await

object Issue23205 {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
    val (promise, source) = Source.maybe[Int].concat(Source(2 to 10)).toMat(BroadcastHub.sink(1))(Keep.both).run()

    val f1 = source.runWith(Sink.cancelled)
    val f2 = source.runWith(Sink.seq)

    // Ensure subscription of Sinks. This is racy but there is no event we can hook into here.
    Thread.sleep(100)
    promise.success(Some(1))
    val result = Await.result(f2, 1 second)
    println(result)
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}

