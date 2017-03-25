/**
  * Created by richard-imaoka on 2/24/2017.
  */

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.persistence.PersistentActor
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}


final case class Cmd(i: Int)
final case class Evt(i: Int)


class PActor() extends PersistentActor {
  var internalState: Int = 0

  def updateState(i: Int): Unit = {
    internalState += i
  }

  override def persistenceId: String = "a"
  override def receiveRecover : Receive = {
    case Evt(i) =>
      updateState(i)
  }

  override def receiveCommand : Receive = {
    case Evt(i) =>
      persist(Evt(i))( _ => { updateState(i); } )
      persist(Evt(i))( _ => { updateState(i); } )
      deferAsync(Evt(i))( _ => { sender() ! internalState } )
  }
}


class PersistentActorMySpec extends TestKit(ActorSystem("MySpec", PersistentActorMySpec.config ))
with ImplicitSender
with WordSpecLike
with Matchers
with BeforeAndAfterAll {

  val storageLocations = List(
    "akka.persistence.journal.leveldb.dir",
    "akka.persistence.journal.leveldb-shared.store.dir",
    "akka.persistence.snapshot-store.local.dir").map(s ⇒ new File(system.settings.config.getString(s)))

  override protected def beforeAll() {
    storageLocations.foreach(FileUtils.deleteDirectory)
  }

  override protected def afterAll() {
    shutdown()
    storageLocations.foreach(FileUtils.deleteDirectory)
  }

  "A persistent actor" must {
    "keep the order of handler execution with deferAsync" in {
      val persistentActor = system.actorOf(Props[PActor])
      persistentActor ! Evt(1)
      expectMsg(2)
      persistentActor ! Evt(1)
      expectMsg(5)
    }
  }

}

object PersistentActorMySpec {
  val config = ConfigFactory.parseString(
    s"""
      akka.actor.serialize-creators = off
      akka.actor.serialize-messages = off
      akka.actor.warn-about-java-serializer-usage = off
      akka.persistence.publish-plugin-commands = on
      akka.persistence.journal.plugin = "akka.persistence.journal.leveldb"
      akka.persistence.journal.leveldb.native = off
      akka.persistence.journal.leveldb.dir = "target/journal-test"
      akka.persistence.snapshot-store.plugin = "akka.persistence.snapshot-store.local"
      akka.persistence.snapshot-store.local.dir = "target/snapshots-test/"
      akka.test.single-expect-default = 10s
    """)
}