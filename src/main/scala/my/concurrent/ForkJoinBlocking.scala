package my.concurrent

import akka.actor.{Actor, ActorSystem, Props}

import scala.concurrent.Future


/**
 * Calling scala.concurrent.blocking() inside Future generates
 * new threads in ForkJoinPool
 */
object ForkJoinBlocking {
  def main(args: Array[String]) = {
    val system = ActorSystem("ForkJoinBlocking")

    implicit val ec = system.dispatcher

    println("started")
    try{
      for(i <- 1 to 100) {
        Future {
          //This will generate a new thread in ForkJoinPool
          scala.concurrent.blocking {
            Thread.sleep(5000)
            println(s"Blocking finished ${i}")
          }
        }
      }
    }
    finally {
      Thread.sleep(10000)
      system.terminate()
    }
  }
}


class SleepActor extends Actor {
  override def receive = {
    case i: Int =>
      scala.concurrent.blocking{
        Thread.sleep(5000)
        println(s"Blocking finished ${i}")
      }
  }
}


/**
 * Calling scala.concurrent.blocking() inside Actor DOES NOT generate new threads
 * for some reason...
 */
object ForkJoinActor {
  def main(args: Array[String]) = {
    val system = ActorSystem("ForkJoinActor")
    val actor = system.actorOf(Props(new SleepActor))

    implicit val ec = system.dispatcher

    println("started")
    try{
      for(i <- 1 to 100) {
        actor ! i
      }
    }
    finally {
      Thread.sleep(10000)
      system.terminate()
    }
  }
}