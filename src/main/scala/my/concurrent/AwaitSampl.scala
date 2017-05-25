package my.concurrent

import java.util.concurrent.ForkJoinPool

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


object AwaitSampl {

  def main(args: Array[String]) = {

    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    val f = Future{
      10
    }


    Await.result(f, Duration(10, SECONDS))

  }
}
