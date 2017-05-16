package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Merge, Sink, Source}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Materialization
  */
object My6thStream {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()


    try {

      val sourceOne = Source(List(1))
      val sourceTwo = Source(List(2))
      val merged = Source.combine(sourceOne, sourceTwo)(Merge(_))

      val mergedResult: Future[Int] = merged.runWith(Sink.fold(0)(_ + _))

      import scala.concurrent.ExecutionContext.Implicits.global
      mergedResult.onComplete {
        case Success(value) => println(s"Future completed with value = ${value}")
        case Failure(ex) => throw ex
      }
    }
    finally{
      system.terminate()
    }
  }
}