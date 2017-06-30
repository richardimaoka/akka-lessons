package my.stream


import akka.Done
import akka.actor.{ActorRef, ActorSystem, NoSerializationVerificationNeeded}
import akka.stream.scaladsl.{Keep, Source}
import akka.stream.{ActorMaterializer, Attributes, Inlet, SinkShape}
import akka.stream.stage.{AsyncCallback, GraphStageLogic, GraphStageWithMaterializedValue, InHandler}
import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

class KeepGoingStageSpec extends TestKit(ActorSystem("KeepGoingStageSpec")) with WordSpecLike with Matchers with BeforeAndAfterAll {
  implicit val materializer = ActorMaterializer()

  trait PingCmd extends NoSerializationVerificationNeeded
  case class Register(probe: ActorRef) extends PingCmd
  case object Ping extends PingCmd
  case object CompleteStage extends PingCmd
  case object FailStage extends PingCmd
  case object Throw extends PingCmd

  trait PingEvt extends NoSerializationVerificationNeeded
  case object Pong extends PingEvt
  case object PostStop extends PingEvt
  case object UpstreamCompleted extends PingEvt
  case object EndOfEventHandler extends PingEvt

  case class PingRef(private val cb: AsyncCallback[PingCmd]) {
    def register(probe: ActorRef): Unit = cb.invoke(Register(probe))
    def ping(): Unit = cb.invoke(Ping)
    def stop(): Unit = cb.invoke(CompleteStage)
    def fail(): Unit = cb.invoke(FailStage)
    def throwEx(): Unit = cb.invoke(Throw)
  }

  class PingableSink(keepAlive: Boolean) extends GraphStageWithMaterializedValue[SinkShape[Int], Future[PingRef]] {
    val shape = SinkShape[Int](Inlet("ping.in"))

    override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[PingRef]) = {
      val promise = Promise[PingRef]()

      val logic = new GraphStageLogic(shape) {
        private var listener: Option[ActorRef] = None

        override def preStart(): Unit = {
          /**
           * Controls whether this stage shall shut down when all its ports are closed, which
           * is the default. In order to have it keep going past that point this method needs
           * to be called with a `true` argument before all ports are closed, and afterwards
           * it will not be closed until this method is called with a `false` argument or the
           * stage is terminated via `completeStage()` or `failStage()`.
           */
          setKeepGoing(keepAlive)

          /**
           * PingRef(getAsyncCallback(onCommand)) holds callback,
           * which 1) can be `invoke()`-ed from outside
           * 2) that will call `onCommand` to do some work inside the stage
           */
          promise.trySuccess(PingRef(getAsyncCallback(onCommand)))
        }

        private def onCommand(cmd: PingCmd): Unit = cmd match {
          case Register(probe) ⇒ listener = Some(probe)
          case Ping            ⇒ listener.foreach(_ ! Pong)
          case CompleteStage ⇒
            completeStage()
            listener.foreach(_ ! EndOfEventHandler)
          case FailStage ⇒
            failStage(new RuntimeException("test"))
            listener.foreach(_ ! EndOfEventHandler)
          case Throw ⇒
            try {
              throw new RuntimeException("test")
            } finally listener.foreach(_ ! EndOfEventHandler)
        }

        setHandler(shape.in, new InHandler {
          override def onPush(): Unit = pull(shape.in)

          // Ignore finish
          override def onUpstreamFinish(): Unit = listener.foreach(_ ! UpstreamCompleted)
        })

        override def postStop(): Unit = listener.foreach(_ ! PostStop)
      }

      /**
       * promise.future is the materialized Future[PingRef]
       */
      (logic, promise.future)
    }
  }

  "A stage with keep-going" must {

    "still be alive after all ports have been closed until explicitly closed" in {
      val ((maybePromise, terminationFut), pingerFuture) = Source.maybe[Int]
        .watchTermination()(Keep.both)
        .toMat(new PingableSink(keepAlive = true))(Keep.both) //PingableSink materializes to Future[PingRef]
        .run()

      /**
       * Materialized to Future[PingRef], then the Future is retrieved to PingRef
       */
      val pinger: PingRef = Await.result(pingerFuture, 3.seconds)
      pinger.register(testActor) //def register(probe: ActorRef): Unit = cb.invoke(Register(probe))
      /**
       * This puts the testActor ref inside the graph
       * (i.e.) `listner` internal variable
       */

      // Before completion
      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      maybePromise.trySuccess(None)
      /**
       * Due to setKeepGoing, the stream is not yet completed
       */
      expectMsg(UpstreamCompleted)
      try {
        val termination: Done = Await.result(terminationFut, 1.seconds)
        println(s"termination = ${termination}, it seems like when upstream is finished, then watchtermination completes.")
        println("i.e. watchTermination() right before a setKeepGoing(true) sink doesn't reflect the sink's completeness")
      }
      catch {
        case e: Exception => println(e)
      }

      expectNoMsg(200.millis)

      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      pinger.stop()
      // PostStop should not be concurrent (i.e. the below messages are received in order)
      // with the event handler. This event here tests this.
      expectMsg(EndOfEventHandler)
      expectMsg(PostStop) //override def postStop(): Unit = listener.foreach(_ ! PostStop)

      /**
       * This below fails
       */
      // pinger.ping()
      // expectMsg(Pong)
    }

    "still be alive after all ports have been closed until explicitly failed" in {
      val (maybePromise, pingerFuture) = Source.maybe[Int].toMat(new PingableSink(keepAlive = true))(Keep.both).run()
      val pinger: PingRef = Await.result(pingerFuture, 3.seconds)

      pinger.register(testActor)

      // Before completion
      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      maybePromise.trySuccess(None)
      expectMsg(UpstreamCompleted)

      expectNoMsg(200.millis)

      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      pinger.fail()
      // PostStop should not be concurrent (i.e. the below messages are received in order)
      // with the event handler. This event here tests this.
      expectMsg(EndOfEventHandler)
      expectMsg(PostStop)

    }

    "still be alive after all ports have been closed until implicitly failed (via exception)" in {
      val (maybePromise, pingerFuture) = Source.maybe[Int].toMat(new PingableSink(keepAlive = true))(Keep.both).run()
      val pinger: PingRef = Await.result(pingerFuture, 3.seconds)

      pinger.register(testActor)

      // Before completion
      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      maybePromise.trySuccess(None)
      expectMsg(UpstreamCompleted)

      expectNoMsg(200.millis)

      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      pinger.throwEx() //exception thrown from onCommand() called inside the stage
      // PostStop should not be concurrent (i.e. the below messages are received in order)
      // with the event handler. This event here tests this.
      expectMsg(EndOfEventHandler)
      expectMsg(PostStop)

    }

    "close down early if keepAlive is not requested" in {
      val (maybePromise, pingerFuture) = Source.maybe[Int].toMat(new PingableSink(keepAlive = false))(Keep.both).run()
      val pinger: PingRef = Await.result(pingerFuture, 3.seconds)

      pinger.register(testActor)

      // Before completion
      pinger.ping()
      expectMsg(Pong)

      pinger.ping()
      expectMsg(Pong)

      maybePromise.trySuccess(None)
      expectMsg(UpstreamCompleted)
      expectMsg(PostStop)

    }
  }
}
