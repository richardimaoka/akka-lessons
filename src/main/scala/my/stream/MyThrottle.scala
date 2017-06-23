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
    println(s"nanosBetweenTokens = ${NumberFormat.getNumberInstance(Locale.US).format(nanosBetweenTokens)}")
    println(s"maximumBurst = ${maximumBurst}")
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
          println(s"delayNanos: ${NumberFormat.getNumberInstance(Locale.US).format(delayNanos)}, cost = ${cost}")

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
          println("pull(in)")
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

  def throttle2(): Unit ={
    /**
     * From the API doc: Sends elements downstream with speed limited to `elements/per`
     * the buffer size > 5, it backpressures upstream (Before Buffer)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before throttle ${x}"); x})
      .via(new MyThrottle(8, 2000 milliseconds, 2, (_: Int) ⇒  2, ThrottleMode.Enforcing))
      .map(x => {println(s"After  throttle ${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(2)
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)
  }


  def main(args: Array[String]): Unit = {
    try {
      //Wrapper("throttle")(throttle)
      Wrapper("throttle2")(throttle2)
    }
    finally{
      system.terminate()
    }
  }
}
