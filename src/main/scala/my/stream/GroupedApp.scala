package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Source}

object GroupedApp {
  implicit val system = ActorSystem("Main")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  def main(args: Array[String]): Unit = {
    try{
      Source(1 to 10).runForeach(println)
      /**
       * Int by Int, 1 elem = 1 Int
       * 1
       * 2
       * 3
       * 4
       * 5
       * 6
       * 7
       * 8
       * 9
       * 10
       */
      Thread.sleep(100)

      /**
       * .grouped() group up elements in Vector(s)
       */
      Source(1 to 10).grouped(5).runForeach(println)
      /**
       * Vector(1, 2, 3, 4, 5)
       * Vector(6, 7, 8, 9, 10)
       */
      Thread.sleep(100)
    }
    finally {
      system.terminate()
    }
  }

}
