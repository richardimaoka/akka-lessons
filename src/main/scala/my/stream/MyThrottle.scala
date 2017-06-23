package my.stream

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, ThrottleMode}
import my.wrapper.Wrapper

import scala.concurrent.duration._

/**
  * Materialization
  */

object MyThrottle {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def throttle(): Unit ={
    val mat1 = Source(1 to 10).throttle(5, 500 milliseconds, 5, i => 1, ThrottleMode.Shaping).runForeach(println(_))
    Thread.sleep(1000)
    println(mat1) //runForeach materializes to Future[Done]

    val mat2 = Source(1 to 10).throttle(5, 500 milliseconds, 5, i => 1, ThrottleMode.Enforcing).runForeach(println(_))
    Thread.sleep(1000)
    println(mat2) //runForeach materializes to Future[Done]
    /**
     * However, due to ThrottleMode.Enforcing, too-fast upstram causes exception in the stream,
     * hence the future fails:
     *   Future(Failure(akka.stream.RateExceededException: Maximum throttle throughput exceeded.))
     */
  }

  def throttleZipWith(): Unit = {
    val source: Source[Int, NotUsed] = Source(1 to 10)
    val factorials = source.scan(BigInt(1))((acc, next) => acc * next)
    factorials
      .throttle(1, 100 milliseconds, 1, ThrottleMode.shaping)
      .runForeach(println)
    Thread.sleep(2000)

    factorials
      .zipWith(source)((num, idx) => s"$idx! = $num")
      .throttle(1, 100 milliseconds, 1, ThrottleMode.shaping)
      .runForeach(println)
    Thread.sleep(2000)
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("throttle")(throttle)
      Wrapper("throttleZipWith")(throttleZipWith)
    }
    finally{
      system.terminate()
    }
  }
}
