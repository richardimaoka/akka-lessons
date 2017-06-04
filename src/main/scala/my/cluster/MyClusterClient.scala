package my.cluster

import akka.actor.{Actor, ActorPath, ActorRef, ActorSystem, Props}
import akka.cluster.client.{ClusterClient, ClusterClientReceptionist, ClusterClientSettings}
import com.typesafe.config.ConfigFactory

class MyPingActor(targetActorPath: String, clusterClient: ActorRef) extends Actor {

  override def preStart(): Unit = {
    clusterClient ! ClusterClient.Send(targetActorPath, "hello", localAffinity = false)
  }

  def receive = {
    case s: String =>
      println(s"Sender received a response: ${s} from ${sender()}")
  }
}

object MyPingActor {
  def props(client: ActorRef): Props = Props(new MyPingActor("/user/pong", client))
}

class MyPongActor extends Actor {
  def receive = {
    case s: String =>
      println(s"Destination received a ping: ${s} from ${sender()}")
      sender ! s
  }
}

/**
 * Weird name, but it means a server reachable for ClusterClient
 */
object MyClusterClientServer {
  val config = ConfigFactory
    .parseString("akka.remote.netty.tcp.port = 2552")
    .withFallback(ConfigFactory.load("cluster-client-app"))

  val systemName = config.getString("clustering.system")

  def main(args: Array[String]): Unit = {
    val system = ActorSystem(systemName, config)
    val actor = system.actorOf(Props[MyPongActor], "pong")

    ClusterClientReceptionist(system).registerService(actor)
  }
}

object MyClusterClient {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory
      .parseString(
        """
          |akka.remote.netty.tcp.port = 2551
          |akka.cluster.seed-nodes = []
        """.stripMargin
      )
      .withFallback(ConfigFactory.parseString("akka.remote.netty.tcp.port=2551"))
      .withFallback(ConfigFactory.load("cluster-client-app"))


    val system = ActorSystem("MyClusterClient", config)

    val initialContacts = Set(ActorPath.fromString("akka.tcp://" + MyClusterClientServer.systemName + "@127.0.0.1:2552/system/receptionist" ))
    val client = system.actorOf(ClusterClient.props(ClusterClientSettings(system).withInitialContacts(initialContacts)), "client")

    system.actorOf(MyPingActor.props(client), "ping")
  }
}
