package my.cats.advanced

import my.wrapper.Wrap

object CartesiansAndApplicativeApp {
  def first(): Unit ={
    import cats.syntax.either._
    def parseInt(str: String): Either[String, Int] =
      Either.catchOnly[NumberFormatException](str.toInt).
        leftMap(_ => s"Couldn't read $str")

    val result = for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield (a + b + c)

    println(result)
    // res1: scala.util.Either[String,Int] = Left(Couldn't read a)
  }

  def firstFuture(): Unit ={
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    lazy val timestamp0 = System.currentTimeMillis

    def getTimestamp: Long = {
      val timestamp = System.currentTimeMillis - timestamp0
      Thread.sleep(100)
      timestamp
    }

    val timestamps = for {
      a <- Future(getTimestamp)
      b <- Future(getTimestamp)
      c <- Future(getTimestamp)
    } yield (a, b, c)

    println(Await.result(timestamps, 1.second))
    // res5: (Long, Long, Long) = (0,106,210)
  }

  def main(args: Array[String]): Unit ={
    Wrap("first")(first)
    Wrap("firstFuture")(firstFuture)
  }
}
