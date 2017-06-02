package my.cluster

import akka.actor.{Actor, ActorPath, ActorSystem, Props}
import akka.cluster.client.{ClusterClient, ClusterClientReceptionist, ClusterClientSettings}
import com.typesafe.config.ConfigFactory

class MyPingPongActor extends Actor {
  def receive = {
    case s: String =>
      println(s"Received: ${s} from ${sender()}")
      sender ! s
  }
}

/**
 * Weird name, but it means a server reachable for ClusterClient
 */
object MyClusterClientServer {
  val config = ConfigFactory.load("cluster-client-app")
  val systemName = config.getString("clustering.system")

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(systemName, config)
    val actor = system.actorOf(Props[MyPingPongActor])

    ClusterClientReceptionist(system).registerService(actor)
  }
}

class MyClusterClient {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("MyClusterClient")
    val initialContacts = Set(
      ActorPath.fromString("akka.tcp://" + MyClusterClientServer.systemName + "@localhost:2552/system/receptionist" )
    )
    val client = system.actorOf(ClusterClient.props(ClusterClientSettings(system).withInitialContacts(initialContacts)))
  }
}
