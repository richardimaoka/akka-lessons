package my.actor

import akka.actor.{Actor, ActorSystem, Props}

class SimpleActor extends Actor {
  override def receive = {
    case s: String => println(s)
  }
}

object ForkJoinActor {
  def main(args: Array[String]) = {
    val system = ActorSystem("ForkJoinActor")

    val actor = system.actorOf(Props(new SimpleActor))
    Thread.sleep(1000)
    actor ! "Hi"

    system.terminate()
  }
}