package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import scala.collection.immutable.Seq

object MapConcat {
  implicit val system = ActorSystem("Main")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  def main(args: Array[String]): Unit = {
    try{
      val s = Source(Seq(Seq(1, 2), Seq(3, 4, 5), Seq(6)))

      println("Source(Seq(Seq(1, 2), Seq(3, 4, 5), Seq(6))).runForeach(println)")
      s.runForeach(println)
      Thread.sleep(200)

      println("flatMapMerge")
      /**
       * flatMapMerge()
       *   Transform each input element into a `Source` of output elements that is
       *   then flattened into the output stream by merging, where at most `breadth`
       *   substreams are being consumed at any given time.
       */
      Source(Seq(Seq(1, 2), Seq(3, 4, 5), Seq(6)))
        .flatMapMerge(5, elem => Source.single(elem))
        .runForeach(println)

    } finally {
      system.terminate()
    }
  }
}
