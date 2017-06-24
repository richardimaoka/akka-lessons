package my.stream

import java.text.NumberFormat
import java.util.Locale

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSource
import akka.stream.{Attributes, _}

import scala.concurrent.Await
import scala.concurrent.duration._

class MyTimer[T] extends GraphStage[FlowShape[T, T]] {
  val in = Inlet[T]("MyTimer.in")
  val out = Outlet[T]("MyTimer.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("MyTimer")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {
      var currentTimestamp: Long = 0
      var howManyElementsProcessed: Int = 0

      override def preStart(): Unit = {
        currentTimestamp = System.nanoTime()
        println("initialized!")
      }

      override def onPush(): Unit = {
        val previousTimestamp = currentTimestamp
        currentTimestamp = System.nanoTime()
        val passed = NumberFormat.getNumberInstance(Locale.US).format(currentTimestamp - previousTimestamp)
        val elem = grab(in)
        push(out, elem)

        if(howManyElementsProcessed == 0)
          println(s"processed element = ${elem}")
        else
          println(s"processed element = ${elem}, ${passed} nanoseconds has passed from the previous element")

        howManyElementsProcessed = howManyElementsProcessed + 1
      }

      override def onPull(): Unit = pull(in)

      setHandlers(in, out, this)
    }
}

object MyThrottle2 {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  /**
   * This constructs and runs a stream where the Sink requests 10 elements and then the Source sends 10 too.
   * (i.e.) NO backpressure from downstream, but there is backpressure from the throttle stage
   */
  def throttleTest(elements: Int, per: FiniteDuration, maxBurst: Int, costPerElement: Int)(): Unit ={
    println("--------------testing a throttle GraphStage in a stream with the following parameters: -------------")

    val nanoBetweenTokens = NumberFormat.getNumberInstance(Locale.US).format(per.toNanos / elements)
    println(s"elements = ${elements}, per = ${per}, maxBurst = ${maxBurst}, costCalc = ${costPerElement}, nanosBetweenTokens = $nanoBetweenTokens")

    val ((sourcePublisher, fut), sinkPublisher) = TestSource.probe[Int]
      .throttle(elements, per, maxBurst, (_: Int) => costPerElement, ThrottleMode.Shaping)
      .via(new MyTimer)
      .watchTermination()(Keep.both)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    /**
     * Request 10 elements from downstream, where the upstream sends 10 elements
     * (i.e.) NO backpressure from downstream
     */
    sinkSubscription.request(10)
    for(i <- 1 to 10)
      sourcePublisher.sendNext(i)
    sourcePublisher.sendComplete()

    try{
      val result = Await.result(fut, 10 seconds)
      println(s"The stream finished with result = ${result}")
    }
    catch{
      case e: Exception => println(s"The stream failed with ${e}")
    }
  }

  /**
   * This constructs and runs a stream where the Sink requests only 5 elements but the Source sends 10.
   * (i.e.) There is backpressure from downstream, as well as backpressure from the throttle stage
   */
  def throttleTestWithDownstreamBackPressure(elements: Int, per: FiniteDuration, maxBurst: Int, costPerElement: Int)(): Unit ={
    println("--------------testing a throttle GraphStage in a stream with the following parameters and backpressure from downstream : -------------")

    val nanoBetweenTokens = NumberFormat.getNumberInstance(Locale.US).format(per.toNanos / elements)
    println(s"elements = ${elements}, per = ${per}, maxBurst = ${maxBurst}, costCalc = ${costPerElement}, nanosBetweenTokens = $nanoBetweenTokens")

    val ((sourcePublisher, fut), sinkPublisher) = TestSource.probe[Int]
      .throttle(elements, per, maxBurst, (_: Int) => costPerElement, ThrottleMode.Shaping)
      .via(new MyTimer)
      .watchTermination()(Keep.both)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    /**
     * Request only 5 elements from downstream, where the upstream sends 10 elements
     * (i.e.) From the 6-th (next to 5) element, there is backpressure from downstream
     */
    sinkSubscription.request(5)  //************ THIS IS THE DIFFERENCE FROM throttleTest *************
    for(i <- 1 to 10)
      sourcePublisher.sendNext(i)
    sourcePublisher.sendComplete()

    /**
     * This Await will fail with timeout exception, since the Future (fut) is Future[Done]
     * which completes when the stream completes, but this stream does not complete as
     * the last 5 elements are still pending due to backpressure from downstream
     */
    try{
      val result = Await.result(fut, 5 seconds)
      println(s"The stream finished with result = ${result}")
    }
    catch{
      case e: Exception => println(s"The stream failed with ${e}")
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      throttleTest(elements = 8, per = 2 seconds, maxBurst = 1, costPerElement = 1)
      throttleTest(elements = 8, per = 2 seconds, maxBurst = 3, costPerElement = 1)
      throttleTest(elements = 8, per = 2 seconds, maxBurst = 5, costPerElement = 1)

      throttleTestWithDownstreamBackPressure(elements = 8, per = 2 seconds, maxBurst = 1, costPerElement = 1)
      throttleTestWithDownstreamBackPressure(elements = 8, per = 2 seconds, maxBurst = 3, costPerElement = 1)
      throttleTestWithDownstreamBackPressure(elements = 8, per = 2 seconds, maxBurst = 5, costPerElement = 1)
    }
    finally {
      system.terminate()
    }
  }
}
