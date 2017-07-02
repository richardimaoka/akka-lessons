package akka.contrib.d3.my

import akka.Done
import akka.actor.{ExtendedActorSystem, Props, SupervisorStrategy}
import akka.pattern.BackoffSupervisor

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

final class LocalStartupTaskProvider( system: ExtendedActorSystem ) extends StartupTaskProvider {

  def startupTask(
    name:                String,
    task:                () ⇒ Future[Done],
    timeout:             FiniteDuration,
    minBackoff:          FiniteDuration,
    maxBackoff:          FiniteDuration,
    randomBackoffFactor: Double
  ): StartupTask = {
    println("LocalStartupTaskProvider: creating startupTask from startupTask()")

    /**
     * This Props creates LocalSingletonManager which holds
     * a single child: childProps = Props(classOf[StartupTaskActor], task, timeout)
     * with backoff supervisory strategy
     */
    val singletonProps = LocalSingletonManager.props(
      BackoffSupervisor.propsWithSupervisorStrategy(
        childProps = Props(classOf[StartupTaskActor], task, timeout),
        childName = name,
        minBackoff = minBackoff,
        maxBackoff = maxBackoff,
        randomFactor = randomBackoffFactor,
        strategy = SupervisorStrategy.stoppingStrategy
      ),
      LocalSingletonManagerSettings("singleton"))
    val singleton = system.actorOf(singletonProps, s"$name-singleton")
    println(s"LocalStartupTaskProvider: created singleton actor = ${singleton}: LocalSingletonManager, which holds StartupTaskActor")

    val singletonProxy = system.actorOf(
      LocalSingletonProxy.props(
        singletonManagerPath = singleton.path.toStringWithoutAddress,
        settings = LocalSingletonProxySettings("singleton", 10000, 1.second)
      ), s"$name-singletonProxy"
    )
    println(s"LocalStartupTaskProvider: created singletonProxy actor = ${singletonProxy}")

    new StartupTask(singletonProxy)
  }

}
