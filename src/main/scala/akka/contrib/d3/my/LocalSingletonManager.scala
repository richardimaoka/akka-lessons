package akka.contrib.d3.my

import akka.actor.{Actor, ActorRef, Props, Terminated, _}

object LocalSingletonManagerSettings {
  def apply(
             singletonName: String = "singleton"
           ): LocalSingletonManagerSettings =
    new LocalSingletonManagerSettings(singletonName)
}

final class LocalSingletonManagerSettings( val singletonName: String) {

  def withSingletonName(name: String): LocalSingletonManagerSettings = copy(singletonName = name)

  def copy(
            singletonName: String = singletonName
          ): LocalSingletonManagerSettings =
    new LocalSingletonManagerSettings(singletonName)
}

object LocalSingletonManager {
  def props(
             singletonProps: Props,
             settings:       LocalSingletonManagerSettings
           ): Props =
    Props(new LocalSingletonManager(singletonProps, settings))

  private case object Start
}

final class LocalSingletonManager(
    singletonProps: Props,
    settings:       LocalSingletonManagerSettings
  ) extends Actor {

  import LocalSingletonManager._
  import settings._

  override def preStart(): Unit = {
    self ! Start
  }

  override def receive: Receive = started

  private def started: Receive = {
    case Start ⇒
      val singleton = createSingleton()
      context.become(active(singleton))
  }

  private def active(singleton: ActorRef): Receive = {
    case Start ⇒ // already started

    case Terminated(ref) if ref == singleton ⇒
      val singleton = createSingleton()
      context.become(active(singleton))
  }

  private def createSingleton(): ActorRef = {
    context watch context.actorOf(singletonProps, singletonName)
  }

}
