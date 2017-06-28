package my.stream

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.TestSource
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.testkit.TestProbe
import akka.util.Timeout
import my.stream.MySinkActor.{AckMessage, CompleteMessage, InitMessage}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object MySinkActor {
  sealed trait MessageTrait
  case object InitMessage extends MessageTrait
  case object CompleteMessage extends MessageTrait
  case object AckMessage extends MessageTrait
}

class MySinkActor extends Actor {
  def receive = {
    case InitMessage =>
      println(s"MySinkActor: received ${InitMessage}")
      sender() ! AckMessage
    case CompleteMessage =>
      println(s"MySinkActor: received ${CompleteMessage}")
    case AckMessage =>
      println(s"MySinkActor: received ${AckMessage}")
    case x: Int =>
      println(s"MySinkActor: received ${(x)}: Int")
      sender() ! AckMessage
    case "boom" =>
      println("received boom!")
      throw new Exception("kabbooom!!")
    case "hey" =>
      println("received hey")
      sender() ! "yo"
  }
}

object MyActorRefWithAck {
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
    println("Expected a dead letters message:")
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

  def watchTerminationStop(): Unit = {
    val probe = TestProbe()
    val fw = system.actorOf(Props(new Fw(probe.ref)), "fw5")

    val (publisher, fut) = TestSource.probe[Int]
      .watchTermination()(Keep.both)
      .to( //to uses Keep.left i.e. publisher is materialized
        Sink.actorRefWithAck(fw, initMessage, ackMessage, completeMessage)
      )
      .run()

    Thread.sleep(100)
    system.stop(fw)
    val result = Await.result(fut, 1 seconds)
    println(s"The stream completed with ${result}") //The stream completes with done
  }

  def test2(): Unit = {
    val actor = system.actorOf(Props(new MySinkActor), "test2")
    /**
     * I just wonder ... how is this Source.queue type inferred?
     * (i.e.) how the compiler determines [T] of Source.queue[T]?
     *
     * CONCLUSION, explicitly supply the type parameter [T] like Source.queue[Int] when using Source.queue
     *
     * -> it seems like it is inferred as T = Any
     * See how the compiler complains by inserting watchTermination() in the middle
     *
     *   [error] C:\....\src\main\scala\my\stream\MyActorRefWithAck.scala:218: polymorphic expression cannot be instantiated to expected type;
     *   [error]  found   : [L, R](L, R) => (L, R)
     *   [error]  required: (akka.stream.scaladsl.SourceQueueWithComplete[?], scala.concurrent.Future[akka.Done]) => ?
     *   [error]       .watchTermination()(Keep.both)
     *   [error]                                ^
     *   [error] C:\Users\nishyu\akka-lessons\src\main\scala\my\stream\MyActorRefWithAck.scala:222: value offer is not a member of Any
     *   [error]     queue.offer(1)
     */
    val queue = Source.queue(10, OverflowStrategy.backpressure)
      //.watchTermination()(Keep.left)
      .to(Sink.actorRefWithAck(actor, InitMessage, AckMessage, CompleteMessage))
      .run()

    queue.offer(1)
    queue.offer(2)
    queue.offer(3)
    queue.complete()
  }

  def testDirectMessages(): Unit = {
    val actor = system.actorOf(Props(new MySinkActor), "testDirectMessages")
    val queue = Source.queue(10, OverflowStrategy.backpressure)
      //.watchTermination()(Keep.left)
      .to(Sink.actorRefWithAck(actor, InitMessage, AckMessage, CompleteMessage))
      .run()

    queue.offer(1)
    queue.offer(2)
    queue.offer(3)
    actor ! 10
    actor ! 100
    actor ! 1000
    queue.complete()
  }

  def test2Terminate(): Unit = {
    try {
      val actor = system.actorOf(Props(new MySinkActor), "test2Terminate")
      val (queue, fut) = Source.queue[Int](10, OverflowStrategy.backpressure)
        .watchTermination()(Keep.both)
        .to(Sink.actorRefWithAck(actor, InitMessage, AckMessage, CompleteMessage))
        .run()

      queue.offer(1)
      queue.offer(2)
      queue.offer(3)
      Thread.sleep(100)
      system.stop(actor)
      Thread.sleep(100)
      queue.complete()

      val result = Await.result(fut, 100 milliseconds)
      println(s"The steram finished with: ${result}")
    } catch {
      case e: Exception => {
        println("The stream failed with the following exception")
        println(e)
      }
    }
  }

  def test2Exception(): Unit = {
    try {
      val actor = system.actorOf(Props(new MySinkActor), "test2Terminate")
      val (queue, fut) = Source.queue[Int](10, OverflowStrategy.backpressure)
        .watchTermination()(Keep.both)
        .to(Sink.actorRefWithAck(actor, InitMessage, AckMessage, CompleteMessage))
        .run()

      queue.offer(1)
      queue.offer(2)
      queue.offer(3)
      actor ! "boom"
      Thread.sleep(100)
      queue.complete()

      implicit val timeout = Timeout(200 milliseconds)
      val actorReply = Await.result(actor ? "hey", 100 milliseconds)
      println(s"actor restarted and replied with ${actorReply}")
      val result = Await.result(fut, 100 milliseconds)
      println(s"The steram finished with: ${result}")
    } catch {
      case e: Exception => {
        println("The stream failed with the following exception")
        println(e)
      }
    }
  }

  def test2PoisonPill(): Unit = {
    try {
      val actor = system.actorOf(Props(new MySinkActor), "test2Failure")
      val (queue, fut) = Source.queue[Int](10, OverflowStrategy.backpressure)
        .watchTermination()(Keep.both)
        .to(Sink.actorRefWithAck(actor, InitMessage, AckMessage, CompleteMessage))
        .run()

      queue.offer(1)
      queue.offer(2)
      queue.offer(3)
      Thread.sleep(100)
      actor ! PoisonPill
      Thread.sleep(100)
      queue.complete()

      val result = Await.result(fut, 100 milliseconds)
      println(s"The stream finished with: ${result}")
    } catch {
      case e: Exception => {
        println("The stream failed with the following exception")
        println(e)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("fixedSource")(fixedSource)
      Wrapper("publisherSource")(publisherSource)
      Wrapper("publisherSourceCancellation")(publisherSourceCancellation)
      Wrapper("watchTermination")(watchTermination)
      Wrapper("watchTerminationStop")(watchTerminationStop)
      Wrapper("test2")(test2)
      Wrapper("test2Terminate")(test2Terminate)
      Wrapper("test2Exception")(test2Exception)
      Wrapper("test2PoisonPill")(test2PoisonPill)
    }
    finally{
      println("terminating the system")
      system.terminate()
    }
  }
}
