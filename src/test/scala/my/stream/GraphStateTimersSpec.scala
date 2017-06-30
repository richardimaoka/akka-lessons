package my.stream

import akka.testkit.TestKit
import akka.actor.{ActorRef, ActorSystem}
import akka.stream.{ActorMaterializer, Attributes}
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.stage._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.Promise
import scala.concurrent.duration._

object GraphStageTimersSpec {
  case object TestSingleTimer
  case object TestSingleTimerResubmit
  case object TestCancelTimer
  case object TestCancelTimerAck
  case object TestRepeatedTimer
  case class Tick(n: Int)

  class SideChannel {
    @volatile var asyncCallback: AsyncCallback[Any] = _
    @volatile var stopPromise: Promise[Option[Nothing]] = _

    def isReady: Boolean = asyncCallback ne null
    /**
     * As asyncCallback is in type = AsyncCallback, invoke (i.e. this ! method )
     * can be called on a different thread from a thread running the stream
     */
    def !(msg: Any) = asyncCallback.invoke(msg)

    def stopStage(): Unit = stopPromise.trySuccess(None)
  }
}

class GraphStageTimersSpec extends TestKit(ActorSystem("GraphStageTimersSpec")) with WordSpecLike with Matchers with BeforeAndAfterAll {
  import GraphStageTimersSpec._

  implicit val materializer = ActorMaterializer()

  class TestStage(probe: ActorRef, sideChannel: SideChannel) extends SimpleLinearGraphStage[Int] {
    override def createLogic(inheritedAttributes: Attributes) = new TimerGraphStageLogic(shape) {
      val tickCount = Iterator from 1

      setHandler(in, new InHandler {
        override def onPush() = push(out, grab(in))
      })

      setHandler(out, new OutHandler {
        override def onPull(): Unit = pull(in)
      })

      override def preStart() = {
        sideChannel.asyncCallback = getAsyncCallback(onTestEvent)
        // trait AsyncCallback[T] { def invoke(t: T): Unit }
        //
        // final def getAsyncCallback[T](handler: T ⇒ Unit): AsyncCallback[T] = {
        //   new AsyncCallback[T] {
        //     override def invoke(event: T): Unit =
        //       interpreter.onAsyncInput(GraphStageLogic.this, event, handler.asInstanceOf[Any ⇒ Unit])
        //   }
        // }
      }

      /**
       * call of onTimer() to be scheduled by scheduleOnce()
       */
      override protected def onTimer(timerKey: Any): Unit = {
        val tick = Tick(tickCount.next())
        println(s"Called onTimer(${timerKey}), sending back tick = ${tick} to testActor")
        probe ! tick
        if (timerKey == "TestSingleTimerResubmit" && tick.n == 1){
          println(s"scheduleOnce(TestSingleTimerResubmit, 500.millis)")
          scheduleOnce("TestSingleTimerResubmit", 500.millis)
        }
        else if (timerKey == "TestRepeatedTimer" && tick.n == 5){
          println(s"scheduleOnce(TestRepeatedTimer, 500.millis)")
          cancelTimer("TestRepeatedTimer")
        }
      }

      private def onTestEvent(event: Any): Unit = event match {
        case TestSingleTimer ⇒
          println("TestStage received TestSingleTimer, scheduling invocation of onTimer(TestSingleTimer) 500 milliseconds later")
          scheduleOnce("TestSingleTimer", 500.millis) //Schedule timer to call [[#onTimer]] after given delay.

        case TestSingleTimerResubmit ⇒
          println("TestStage received TestSingleTimerResubmit, scheduling invocation of onTimer(TestSingleTimerResubmit) 500 milliseconds later")
          scheduleOnce("TestSingleTimerResubmit", 500.millis)

        case TestCancelTimer ⇒
          println("TestStage received TestCancelTimer, scheduling invocation of onTimer(TestCancelTimer) 1 millisecond later")
          scheduleOnce("TestCancelTimer", 1.milli)
          // Likely in mailbox but we cannot guarantee

          println("And cancel onTimer(TestCancelTimer)")
          cancelTimer("TestCancelTimer")

          println("Sending back TestCancelTimerAck to testActor")
          probe ! TestCancelTimerAck

          println("Scheduling again invocation of onTimer(TestCancelTimer) 500 milliseconds later")
          scheduleOnce("TestCancelTimer", 500.milli)

        case TestRepeatedTimer ⇒
          println("TestStage received TestRepeatedTimer, scheduling invocation of onTimer(TestRepeatedTimer) 100 milliseconds later")
          schedulePeriodically("TestRepeatedTimer", 100.millis)
      }
    }
  }

  "GraphStage timer support" must {

    def setupIsolatedStage: SideChannel = {
      val channel = new SideChannel
      val stopPromise = Source.maybe[Nothing].via(new TestStage(testActor, channel)).to(Sink.ignore).run()
      channel.stopPromise = stopPromise
      awaitCond(channel.isReady) //def isReady: Boolean = asyncCallback ne null
      channel
    }

    "receive single-shot timer" in {
      /**
       * SideChannel has a ! method, which calls AsyncCallback's invoke
       * i.e interruption (re-enter) from another thread into the stream-running thread
       */
      val driver: SideChannel = setupIsolatedStage
      within(2.seconds) {
        //min = 500 mills is for scheduleOnce(..., 500 mills) <- make sure this scheduleOnce is called
        within(500.millis, 1.second) {
          /**
           * As asyncCallback is in type = AsyncCallback, invoke (i.e. this ! method )
           * can be called on a different thread from a thread running the stream
           */
          println("Sending TestSingleTimer to SideChannel, by AsyncCallback.invoke()")
          driver ! TestSingleTimer //def !(msg: Any) = asyncCallback.invoke(msg)
          expectMsg(Tick(1))
          println("Tick(1) is received by the test thread")
        }
        expectNoMsg(1.second)
      }

      driver.stopStage()
      println()
    }

    "resubmit single-shot timer" in {
      val driver: SideChannel = setupIsolatedStage

      within(2.5.seconds) {
        //min = 500 mills is for scheduleOnce(..., 500 mills) <- make sure this scheduleOnce is called
        within(500.millis, 1.second) {
          println("Sending TestSingleTimerResubmit to SideChannel, by AsyncCallback.invoke()")
          driver ! TestSingleTimerResubmit
          expectMsg(Tick(1))
          println("Tick(1) is received by the test thread")
        }
        within(1.second) {
          expectMsg(Tick(2))
          println("Tick(2) is received by the test thread")
        }
        expectNoMsg(1.second)
      }

      driver.stopStage()
      println()
    }

    "correctly cancel a named timer" in {
      val driver: SideChannel = setupIsolatedStage

      driver ! TestCancelTimer
      within(500.millis) {
        expectMsg(TestCancelTimerAck)
      }
      within(300.millis, 1.second) {
        expectMsg(Tick(1))
      }
      expectNoMsg(1.second)

      driver.stopStage()
    }

    "receive and cancel a repeated timer" in {
      val driver: SideChannel = setupIsolatedStage

      driver ! TestRepeatedTimer
      val seq = receiveWhile(2.seconds) {
        case t: Tick ⇒ t
      }
      seq should have length 5
      expectNoMsg(1.second)

      driver.stopStage()
    }
  }
}
