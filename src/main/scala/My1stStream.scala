package p1

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._

object My1stStream {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("MyActorSystem")
    implicit val materializer = ActorMaterializer()

    val sourceFromRange = Source(1 to 10)
    sourceFromRange runForeach{ i => println(i) }

    system.terminate()
  }
}