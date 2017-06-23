package my.stream

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.TestSource
import akka.testkit.TestProbe

import scala.util.{Failure, Success}

object MyactorRefWithAck {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val initMessage = "start"
  val completeMessage = "done"
  val ackMessage = "ack"

  class Fw(testActor: ActorRef) extends Actor {
    /**
     * `initMessage`:
     *   (inside stream) ---> `initMessage` ---> self ---> (testActor)
     *              ack <-------------------------|
     *
     * `completeMessage`:
     *   (inside stream) ---> `completeMessage` ---> (testActor)
     *
     *  msg: Int
     *   (inside stream) ---> msg ---> self ---> (testActor)
     *              ack <---------------|
     */
    def receive = {
      case `initMessage` ⇒        //this initMessage is same as one passed to Sink.actorRefWithAck
        println(s"Fw ${self.path} received initMessage = ${initMessage}")
        sender() ! ackMessage     //this ackMessage is same as one passed to Sink.actorRefWithAck
        testActor forward initMessage
      case `completeMessage` ⇒
        println(s"Fw ${self.path} received completeMessage = ${completeMessage}")
        testActor forward completeMessage
      case msg: Int ⇒
        println(s"Fw ${self.path} received msg: ${msg}")
        sender() ! ackMessage
        testActor forward msg
    }
  }

  def wrapper(callerFunctionName: String = "f")(unitFunction: () => Unit): Unit = {
    unitFunction()
    Thread.sleep(50)
    println("\n\n")
  }

  /**
   * Using Source(List(1, 2, 3))
   */
  def fixedSource(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw1")

    Source(List(1, 2, 3))
      .runWith(
        Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
      )

    probe.expectMsg(initMessage)
    probe.expectMsg(1)
    probe.expectMsg(2)
    probe.expectMsg(3)
    probe.expectMsg(completeMessage)
  }

  /**
   * Using TestSource.probe[Int]
   */
  def publisherSource(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw2")

    val publisher = TestSource.probe[Int].to( //to uses Keep.left i.e. publisher is materialized
      Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
    ).run()

    println(s"publisher = ${publisher}")

    publisher.sendNext(1)
    probe.expectMsg(initMessage) //initMessage AND 1 are expected
    probe.expectMsg(1)

    publisher.sendNext(2)
    probe.expectMsg(2)
    publisher.sendNext(3)
    probe.expectMsg(3)
    publisher.sendNext(4)
    probe.expectMsg(4)

    publisher.sendComplete()
    probe.expectMsg(completeMessage)
  }

  def publisherSourceCancellation(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw3")

    val publisher = TestSource.probe[Int].to( //to uses Keep.left i.e. publisher is materialized
      Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
    ).run()
    publisher.sendNext(1)
    probe.expectMsg(initMessage)
    probe.expectMsg(1)
    system.stop(fw)
    /**
     * After stopping the actor fw, a deadLetters warning like below is seen,
     * and this is also happening in akka.stream.scaladsl.ActorRefBackpressureSinkSpec -> "cancel stream when actor terminates"
     *
     * [INFO] [06/23/2017 09:19:38.874]
     * [default-akka.actor.default-dispatcher-3] [akka://default/user/fw3]
     *
     *   Message [akka.actor.Status$Failure]
     *   from Actor[akka://default/user/StreamSupervisor-0/$$c#371709300]
     *   to Actor[akka://default/user/fw3#-1467649245] was not delivered.
     *   [1] dead letters encountered. This logging can be turned off or adjusted with configuration settings 'akka.log-dead-letters' and 'akka.log-dead-letters-during-shutdown'.
     *
     */
    publisher.expectCancellation()
  }

  def watchTermination(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw4")

    val (publisher, fut) = TestSource.probe[Int]
      .watchTermination()(Keep.both)
      .to( //to uses Keep.left i.e. publisher is materialized
        Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
      )
      .run()

    import scala.concurrent.ExecutionContext.Implicits.global
    fut.onComplete{
      case Success(x) => println(s"fut.onComplete success by ${x}")
      case Failure(x) => println(s"fut.onComplete failed by ${x}")
    }

    publisher.sendNext(1)
    probe.expectMsg(initMessage)
    probe.expectMsg(1)

    publisher.sendComplete() //After this, fut is supposed to complete with Future[Done]
  }

  def watchTerminationFailure(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw5")

    val (publisher, fut) = TestSource.probe[Int]
      .watchTermination()(Keep.both)
      .to( //to uses Keep.left i.e. publisher is materialized
        Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
      )
      .run()

    import scala.concurrent.ExecutionContext.Implicits.global
    fut.onComplete{
      case Success(x) => println(s"fut.onComplete success by ${x}")
      case Failure(x) => println(s"fut.onComplete failed by ${x}")
    }

    system.stop(fw) //After this, fut is supposed to complete with Failure
  }

  def main(args: Array[String]): Unit = {
    try {
      wrapper("fixedSource")(fixedSource)
      wrapper("publisherSource")(publisherSource)
      wrapper("publisherSourceCancellation")(publisherSourceCancellation)
      wrapper("watchTermination")(watchTermination)
      wrapper("watchTerminationFailure")(watchTerminationFailure)
    }
    finally{
      println("terminating the system")
      system.terminate()
    }
  }
}
