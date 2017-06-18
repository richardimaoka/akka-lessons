package my.actor

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorSystem, DeathPactException, OneForOneStrategy, PoisonPill, Props, SupervisorStrategy}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._


class Parent extends Actor {
  override def receive = {
    case name: String =>
      val ref = context.actorOf(Props[Child], name)
      sender() ! ref
  }

  /**
    * User overridable definition the strategy to use for supervising
    * child actors.
    *
    * SupervisorStrategy.defaultStrategy
    */
  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(){
    case m: ActorInitializationException ⇒ {
      println(s"Parent: supervision strategy received: ${m}, and `Stop`-ing the child")
      Stop
    }
    case m: ActorKilledException ⇒ {
      println(s"Parent: supervision strategy received: ${m}, and `Stop`-ing the child")
      Stop
    }
    case m: DeathPactException ⇒ {
      println(s"Parent: supervision strategy received: ${m}, and `Stop`-ing the child")
      Stop
    }
    case m: Exception => {
      println(s"Parent: supervision strategy received: ${m}, and `Restart`-ing the child")
      Restart
    }
  }
}


class Child extends Actor {

  println("Child : constructed")

  override def receive = {
    case "normal" =>
      println("Child : Received 'normal' message, so just printing it out .")
    case "kaboom" =>
      println("Child : Received kaboom")
      throw new Exception("kaboom!!")
    case "stop" =>
      println("Child : Received 'stop', so calling context.stop()")
      context.stop(self)
    case m: Any =>
      println(s"Child : Received: ${m}")
  }

  override def preStart() = {
    println(s"Child : preStart() called, with hashCode =${hashCode} (supposedly hashCode = object's memory address)")
  }

  override def postStop(): Unit = {
    println("Child : postStop() called")
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    println(s"Child : preRestart() called for ${reason} and ${message}")
    super.preRestart(reason, message)
  }

  override def postRestart(reason: Throwable): Unit = {
    println(s"Child : postRestart() called for ${reason}")
    super.postRestart(reason)
  }
}

object MyActorIncarnation {
  def main(args: Array[String]): Unit = {
    implicit val system  = ActorSystem("MyActorIncarnation")
    implicit val timeout = Timeout(1 seconds)

    try{

      println(
        """
          |*********************************************
          |
          |  Let's see how restart works
          |
          |*********************************************
          |
        """.stripMargin
      )

      val parent = system.actorOf(Props[Parent], "parent")
      val fut    = parent ? "child"
      Thread.sleep(500) //wait until actors are fully created calling the constructor and preStart()

      val child  = Await.result(fut, 1 second).asInstanceOf[ActorRef]
      println(s"main  : child = ${child} (path#-incarnation uid)")

      child ! "kaboom"
      Thread.sleep(500) // wait until restart is fully done
      child ! "normal"
      Thread.sleep(100) // wait until the "normal" message is processed

      println(
        """
          |You must have seen:
          |  * Child actor's hashCode changed after restart
          |  * Child actor's constructor and preStart() are both called on restart
          |  * The Child actor handled the "normal" message after restart
        """.stripMargin
      )

      println(
        """
          |*********************************************
          |
          |  Let's see how stop works
          |
          |*********************************************
          |
        """.stripMargin
      )

      child ! "stop"
      Thread.sleep(500) // wait until stop is fully done

      val fut2    = parent ? "child"
      val child2  = Await.result(fut2, 1 second).asInstanceOf[ActorRef]
      println(s"main  : child2 = ${child2} (path#-incarnation uid)")

      Thread.sleep(500) // wait until the new actor incarnation is fully ready

      child ! "normal"  // sending to an old, invalid ActorRef
      Thread.sleep(100) // wait "normal" reaches at deadLetters

      println(
        """
          |You must have seen:
          |  * Child actor's hashCode changed in the new incarnation (after complete "stop")
          |  * Child actor's constructor and preStart() are both called in the new incarnation
          |  * AcreRef for the Child actor had a new uid (i.e.) confirming the new incarnation
          |  * Sending "normal" to the old ActorRef will end up in deadLetters
        """.stripMargin
      )

      println(
        """
          |*********************************************
          |
          |  Receiving PoisonPill does the same thing as
          |  calling context.stop() for the child
          |
          |*********************************************
          |
        """.stripMargin
      )

      child ! PoisonPill
      Thread.sleep(1000)

    } finally {
      println("main:  terminating the system")
      system.terminate()
    }
  }
}
