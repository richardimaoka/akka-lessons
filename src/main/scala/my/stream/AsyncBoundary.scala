package my.stream

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ThrottleMode}
import akka.stream.scaladsl.{Keep, Sink, Source}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

object AsyncBoundary {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit = {
    Source(List(1, 2, 3))
      .map(_ + 1).async
      .map(_ * 2)
      .to(Sink.foreach(println(_)))
      .run()
  }

  def testWithThrottleFusing(): Unit = {
    val fut = Source(1 to 5)
      .via(new MyCounter[Int]("upstream  "))
      .throttle(2, 10 milliseconds, 0, ThrottleMode.Shaping)
      .via(new MyCounter[Int]("midstream "))
      .throttle(1, 100 milliseconds, 0, ThrottleMode.Shaping)
      .via(new MyCounter[Int]("downstream"))
      .watchTermination()(Keep.right)
      .to(Sink.ignore)
      .run()
    val result = Await.result(fut, 5 seconds)
    println(s"$result = {result}")
  }

  def testWithThrottleAsync(): Unit = {
    val fut = Source(1 to 5)
      .via(new MyCounter[Int]("upstream  "))
      .throttle(2, 10 milliseconds, 0, ThrottleMode.Shaping).async
      .via(new MyCounter[Int]("midstream "))
      .throttle(1, 100 milliseconds, 0, ThrottleMode.Shaping).async
      .via(new MyCounter[Int]("downstream"))
      .watchTermination()(Keep.right)
      .to(Sink.ignore)
      .run()

    val result = Await.result(fut, 5 seconds)
    println(s"$result = {result}")
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
      Wrapper("testWithThrottleFusing")(testWithThrottleFusing)
      Wrapper("testWithThrottleAsync")(testWithThrottleAsync)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
