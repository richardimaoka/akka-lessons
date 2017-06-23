package my.stream

import java.text.NumberFormat
import java.util.Locale

import akka.NotUsed
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.ThrottleMode.{Enforcing, Shaping}
import akka.stream._
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.stage._
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSource
import akka.util.NanoTimeTokenBucket
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

class MyThrottle[T](
                   val cost:            Int,
                   val per:             FiniteDuration,
                   val maximumBurst:    Int,
                   val costCalculation: (T) ⇒ Int,
                   val mode:            ThrottleMode)
  extends GraphStage[FlowShape[T,T]] {
  require(cost > 0, "cost must be > 0")
  require(per.toNanos > 0, "per time must be > 0")
  require(!(mode == ThrottleMode.Enforcing && maximumBurst < 0), "maximumBurst must be > 0 in Enforcing mode")
  require(per.toNanos >= cost, "Rates larger than 1 unit / nanosecond are not supported")

  val in = Inlet[T](Logging.simpleName(this) + ".in")
  val out = Outlet[T](Logging.simpleName(this) + ".out")
  override val shape = FlowShape(in, out)

  // There is some loss of precision here because of rounding, but this only happens if nanosBetweenTokens is very
  // small which is usually at rates where that precision is highly unlikely anyway as the overhead of this stage
  // is likely higher than the required accuracy interval.
  private val nanosBetweenTokens = per.toNanos / cost
  private val timerName: String = "ThrottleTimer"

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new TimerGraphStageLogic(shape) {
    //println(s"nanosBetweenTokens = ${NumberFormat.getNumberInstance(Locale.US).format(nanosBetweenTokens)}")
    //println(s"maximumBurst = ${maximumBurst}")
    private val tokenBucket = new NanoTimeTokenBucket(maximumBurst, nanosBetweenTokens)

    var willStop = false
    var currentElement: T = _
    val enforcing = mode match {
      case Enforcing ⇒ true
      case Shaping   ⇒ false
    }

    override def preStart(): Unit = tokenBucket.init()

    // This scope is here just to not retain an extra reference to the handler below.
    // We can't put this code into preRestart() because setHandler() must be called before that.
    {
      val handler = new InHandler with OutHandler {
        override def onUpstreamFinish(): Unit =
          if (isAvailable(out) && isTimerActive(timerName)) willStop = true
          else completeStage()

        override def onPush(): Unit = {
          val elem = grab(in)
          val cost = costCalculation(elem)
          val delayNanos = tokenBucket.offer(cost)
          println(s"delayNanos: ${NumberFormat.getNumberInstance(Locale.US).format(delayNanos)}, elem = ${elem}, cost(Calc'd) = ${cost}")

          if (delayNanos == 0L) push(out, elem)
          else {
            if (enforcing) failStage(new RateExceededException("Maximum throttle throughput exceeded."))
            else {
              currentElement = elem
              scheduleOnce(timerName, delayNanos.nanos)
            }
          }
        }

        override def onPull(): Unit = {
          //println("pull(in)")
          pull(in)
        }
      }

      setHandler(in, handler)
      setHandler(out, handler)
      // After this point, we no longer need the `handler` so it can just fall out of scope.
    }

    override protected def onTimer(key: Any): Unit = {
      push(out, currentElement)
      currentElement = null.asInstanceOf[T]
      if (willStop) completeStage()
    }

  }

  override def toString = "Throttle"
}

object MyThrottle {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def throttle(): Unit ={
    /**
     * 5 / 500 milliseconds = 10 elements / 1 second
     */
    val mat1 = Source(1 to 10).throttle(5, 500 milliseconds, 2, i => 1, ThrottleMode.Shaping).runForeach(println(_))
    Thread.sleep(2000)
    println(mat1) //runForeach materializes to Future[Done]

    val mat2 = Source(1 to 10).throttle(5, 500 milliseconds, 2, i => 1, ThrottleMode.Enforcing).runForeach(println(_))
    Thread.sleep(2000)
    println(mat2) //runForeach materializes to Future[Done]
    /**
     * However, due to ThrottleMode.Enforcing, too-fast upstram causes exception in the stream,
     * hence the future fails:
     *   Future(Failure(akka.stream.RateExceededException: Maximum throttle throughput exceeded.))
     */
  }

  def throttleZipWith(): Unit = {
    val source: Source[Int, NotUsed] = Source(1 to 10)
    val factorials = source.scan(BigInt(1))((acc, next) => acc * next)
    factorials
      .throttle(1, 100 milliseconds, 1, ThrottleMode.shaping)
      .runForeach(println)
    Thread.sleep(2000)

    factorials
      .zipWith(source)((num, idx) => s"$idx! = $num")
      .throttle(1, 100 milliseconds, 1, ThrottleMode.shaping)
      .runForeach(println)
    Thread.sleep(2000)
  }

  def throttle2(cost: Int, per: FiniteDuration, maxBurst: Int, costCalc: Int)(): Unit ={
    val nanoBetweenTokens = NumberFormat.getNumberInstance(Locale.US).format(per.toNanos / cost)
    println(s"cost = ${cost}, per = ${per}, maxBurst = ${maxBurst}, costCalc = ${costCalc}, nanosBetweenTokens = $nanoBetweenTokens}")

    /**
     * From the API doc: Sends elements downstream with speed limited to `elements/per`
     * the buffer size > 5, it backpressures upstream (Before Buffer)
     */
    val ((sourcePublisher, fut), sinkPublisher) = TestSource.probe[Int]
      //.map(x => {println(s"Before throttle ${x}"); x})
      .via(new MyThrottle(cost, per, maxBurst, (_: Int) ⇒ costCalc, ThrottleMode.Shaping))
      //.map(x => {println(s"After  throttle ${x}"); x})
      .watchTermination()(Keep.both)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(10)
    for(i <- 1 to 10)
      sourcePublisher.sendNext(i)
    sourcePublisher.sendComplete()

    val result = Await.result(fut, 20 seconds)
    println(s"result = ${result}")
  }


  def main(args: Array[String]): Unit = {
    try {
      Wrapper("throttle2")(throttle2(3, 2 seconds, 1, 1))
      /**
       * ----throttle2-----------------------------------------------
       * cost = 3, per = 2 seconds, maxBurst = 1, costCalc = 1, nanosBetweenTokens = 666,666,666}
       * delayNanos: 0, elem = 1, cost(Calc'd) = 1           <----- the 1st element has no delayNanos
       * delayNanos: 654,398,196, elem = 2, cost(Calc'd) = 1 <----- from the 2nd, delayNanos = (nearly) nanosBetweenTokens
       * delayNanos: 635,666,628, elem = 3, cost(Calc'd) = 1
       * delayNanos: 648,665,538, elem = 4, cost(Calc'd) = 1
       * delayNanos: 647,081,762, elem = 5, cost(Calc'd) = 1
       * delayNanos: 643,085,818, elem = 6, cost(Calc'd) = 1
       * delayNanos: 642,251,714, elem = 7, cost(Calc'd) = 1
       * delayNanos: 636,676,202, elem = 8, cost(Calc'd) = 1
       * delayNanos: 642,899,826, elem = 9, cost(Calc'd) = 1
       * delayNanos: 639,450,736, elem = 10, cost(Calc'd) = 1
       * result = Done
       */

      Wrapper("throttle2")(throttle2(3, 2 seconds, 2, 1))
      /**
       * ----throttle2-----------------------------------------------
       * cost = 3, per = 2 seconds, maxBurst = 2, costCalc = 1, nanosBetweenTokens = 666,666,666}
       * delayNanos: 0, elem = 1, cost(Calc'd) = 1
       * delayNanos: 0, elem = 2, cost(Calc'd) = 1           <----- delayNanos = 0 for 2 elems, as maxBurst = 2
       * delayNanos: 648,751,774, elem = 3, cost(Calc'd) = 1
       * delayNanos: 652,537,989, elem = 4, cost(Calc'd) = 1
       * delayNanos: 640,396,017, elem = 5, cost(Calc'd) = 1
       * delayNanos: 635,426,852, elem = 6, cost(Calc'd) = 1
       * delayNanos: 644,962,851, elem = 7, cost(Calc'd) = 1
       * delayNanos: 638,887,958, elem = 8, cost(Calc'd) = 1
       * delayNanos: 647,209,159, elem = 9, cost(Calc'd) = 1
       * delayNanos: 646,610,924, elem = 10, cost(Calc'd) = 1
       * result = Done
       */

      Wrapper("throttle2")(throttle2(3, 2 seconds, 3, 1))
      /**
       * ----throttle2-----------------------------------------------
       * cost = 3, per = 2 seconds, maxBurst = 3, costCalc = 1, nanosBetweenTokens = 666,666,666}
       * delayNanos: 0, elem = 1, cost(Calc'd) = 1
       * delayNanos: 0, elem = 2, cost(Calc'd) = 1
       * delayNanos: 0, elem = 3, cost(Calc'd) = 1            <----- delayNanos = 0 for 3 elems, as maxBurst = 3
       * delayNanos: 650,203,643, elem = 4, cost(Calc'd) = 1
       * delayNanos: 642,999,886, elem = 5, cost(Calc'd) = 1
       * delayNanos: 636,589,970, elem = 6, cost(Calc'd) = 1
       * delayNanos: 644,135,059, elem = 7, cost(Calc'd) = 1
       * delayNanos: 642,790,457, elem = 8, cost(Calc'd) = 1
       * delayNanos: 636,439,734, elem = 9, cost(Calc'd) = 1
       * delayNanos: 644,724,578, elem = 10, cost(Calc'd) = 1
       * result = Done
       */

      Wrapper("throttle2")(throttle2(3, 2 seconds, 3, 2))
      /**
       * ----throttle2-----------------------------------------------
       * cost = 3, per = 2 seconds, maxBurst = 3, costCalc = 2, nanosBetweenTokens = 666,666,666}
       * delayNanos: 0, elem = 1, cost(Calc'd) = 2
       * delayNanos: 654,376,263, elem = 2, cost(Calc'd) = 2
       * delayNanos: 1,312,084,731, elem = 3, cost(Calc'd) = 2 <----- from the 3rd, delayNanos = (nearly) costCalc x nanosBetweenTokens
       * delayNanos: 1,306,896,823, elem = 4, cost(Calc'd) = 2
       * delayNanos: 1,310,710,681, elem = 5, cost(Calc'd) = 2
       * delayNanos: 1,304,131,300, elem = 6, cost(Calc'd) = 2
       * delayNanos: 1,307,082,209, elem = 7, cost(Calc'd) = 2
       * delayNanos: 1,313,079,879, elem = 8, cost(Calc'd) = 2
       * delayNanos: 1,312,314,582, elem = 9, cost(Calc'd) = 2
       * delayNanos: 1,306,504,402, elem = 10, cost(Calc'd) = 2
       * result = Done
       */

      Wrapper("throttle2")(throttle2(3, 2 seconds, 3, 3))
      /**
       * ----throttle2-----------------------------------------------
       * cost = 3, per = 2 seconds, maxBurst = 3, costCalc = 3, nanosBetweenTokens = 666,666,666}
       * delayNanos: 0, elem = 1, cost(Calc'd) = 3
       * delayNanos: 1,988,257,951, elem = 2, cost(Calc'd) = 3 <----- from the 2nd, delayNanos = (nearly) costCalc x nanosBetweenTokens
       * delayNanos: 1,973,537,048, elem = 3, cost(Calc'd) = 3
       * delayNanos: 1,976,589,215, elem = 4, cost(Calc'd) = 3
       * delayNanos: 1,976,252,086, elem = 5, cost(Calc'd) = 3
       * delayNanos: 1,976,146,619, elem = 6, cost(Calc'd) = 3
       * delayNanos: 1,976,220,232, elem = 7, cost(Calc'd) = 3
       * delayNanos: 1,976,235,554, elem = 8, cost(Calc'd) = 3
       * delayNanos: 1,974,231,120, elem = 9, cost(Calc'd) = 3
       * delayNanos: 1,973,754,574, elem = 10, cost(Calc'd) = 3
       * result = Done
       */
    }
    finally{
      system.terminate()
    }
  }
}
