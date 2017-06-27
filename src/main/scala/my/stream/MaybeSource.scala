package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

object MaybeSource {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def testSuccessSome(): Unit ={
    val promise = Source.maybe[Int].toMat(Sink.foreach(println(_)))(Keep.left).run()

    /**
     * 1 will be printed by Sink.foreach(println(_))
     */
    promise.success(Some(1))
  }

  def testSuccessNone(): Unit ={
    val promise = Source.maybe[Int].toMat(Sink.foreach(println(_)))(Keep.left).run()

    /**
     * Nothing will be printed
     */
    promise.success(None)
  }

  def testFailure(): Unit ={
    val (promise, fut) = Source.maybe[Int].toMat(Sink.foreach(println(_)))(Keep.both).run()

    /**
     * Fail with Exception("I am too mad")
     */
    promise.failure(new Exception("I am too mad"))
    try{
      Await.result(fut, 1 second)
      println("this line should never be reached as the above should throw an exception")
    }
    catch {
      case e: Exception => println(e)
    }

  }


  def main(args: Array[String]): Unit = {
    try {
      Wrapper("testSuccessSome")(testSuccessSome)
      Wrapper("testSuccessNone")(testSuccessNone)
      Wrapper("testFailure")(testFailure)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
