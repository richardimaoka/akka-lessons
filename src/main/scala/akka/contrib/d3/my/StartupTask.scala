package akka.contrib.d3.my


import akka.Done
import akka.actor.{Actor, ActorLogging, ActorRef, _}
import akka.util.Timeout
import akka.pattern._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Failure

class StartupTask(actorRef: ActorRef) {

  import StartupTaskActor._

  def execute()(implicit timeout: Timeout): Future[Done] = {
    println(s"StartupTask: sends Execute to ${actorRef}")
    (actorRef ? Execute).mapTo[Done]
  }

}

object StartupTaskActor {
  case object Execute
}

class StartupTaskActor(
    task:    () ⇒ Future[Done],
    timeout: FiniteDuration
  ) extends Actor with ActorLogging {

  import StartupTaskActor._

  import context.dispatcher

  override def preStart(): Unit = {
    implicit val askTimeout = Timeout(timeout)
    /**
     * Execute the `started: Receive` callback by sending Execute,
     * self ? Execute results in completed Future[Done] passed in as parameter, due to task() in start
     *
     * Then send Done to self, to be handled by `executing: Receive`
     */
    self ? Execute pipeTo self
    ()
  }

  def receive: Receive = started

  def started: Receive = {
    case Execute ⇒
      log.info(s"Executing start task ${self.path.name}.")
      task() pipeTo self //the passed in task(): Future[Done] is sent back to self
      context become executing(List(sender()))
  }

  def executing(outstandingRequests: List[ActorRef]): Receive = {
    case Execute ⇒
      log.info(s"Start task ${self.path.name}.")
      context become executing(sender() :: outstandingRequests)

    case Done ⇒
      log.info(s"Start task ${self.path.name} finished successfully.")
      outstandingRequests foreach { requester ⇒
        requester ! Done
      }
      context become executed

    case failure @ Failure(e) ⇒
      outstandingRequests foreach { requester ⇒
        requester ! failure
      }
      // If we failed to prepare, crash and let our parent handle this
      throw e
  }

  def executed: Receive = {
    case Execute ⇒
      log.info(s"StartupTaskActor: received Execute in executed()")
      sender() ! Done

    case Done ⇒
      log.info(s"StartupTaskActor: received Done in executed()")
    // We do expect to receive Done once executed since we initially asked ourselves to execute
  }

}
