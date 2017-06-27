package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import my.wrapper.Wrapper

object BareBone {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
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
