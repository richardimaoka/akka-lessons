package my.stream

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink}
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSource
import akka.stream.{ActorMaterializer, OverflowStrategy}
import my.wrapper.Wrapper

import scala.util.{Failure, Success}

object MyBuffer {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def bufferTestBackPressure(): Unit ={
    /**
     * The buffer() in the middle "queues up" elements up to 5
     * the buffer size > 5, it backpressures upstream (Before Buffer)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer ${x}"); x})
      .buffer(5, OverflowStrategy.backpressure)
      .map(x => {println(s"After  buffer ${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(5)
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 10 elements = 5(bufferSize) + 5(requested)
        |After  buffer must be at 5 = 5 (requested)
      """.stripMargin)

    sinkSubscription.request(5)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 15 elements = 5(bufferSize) + (5+5)(requested)
        |After  buffer must be at 5 = (5+5) (requested)
      """.stripMargin)
  }

  def bufferTestDropHead(): Unit ={
    /**
     * From the API Comment about the overflow strategy:
     *   Backpressure - backpressures when buffer is full
     *   DropHead, DropTail, DropBuffer - never backpressures
     *   Fail - fails the stream if buffer gets full
     *
     * The buffer() in the middle "queues up" elements up to 5
     * but no backpressure to upstream ... (continue)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer ${x}"); x})
      .buffer(5, OverflowStrategy.dropHead)
      .map(x => {println(s"After  buffer ${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(5)
    /**
     *  ... (continued) no backpressure to upstream, hence all the 20 elements
     *  are consumed immediately in the "Before buffer" stage
     */
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 20 = everything from upstream
        |After  buffer must be at 5 = 5 (requested)
      """.stripMargin)

    sinkSubscription.request(5)
    Thread.sleep(100)

    println(
      """
        |After buffer must have processed from 16 to 20, and discarded 6 to 15, due to OiverflowStrategy.dropHead
      """.stripMargin)
  }

  def bufferTestDropTail(): Unit ={
    /**
     * From the API Comment about the overflow strategy:
     *   Backpressure - backpressures when buffer is full
     *   DropHead, DropTail, DropBuffer - never backpressures
     *   Fail - fails the stream if buffer gets full
     *
     * The buffer() in the middle "queues up" elements up to 5
     * but no backpressure to upstream ... (continue)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer ${x}"); x})
      .buffer(5, OverflowStrategy.dropTail)
      .map(x => {println(s"After buffer ${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(5)
    /**
     *  ... (continued) no backpressure to upstream, hence all the 20 elements
     *  are consumed immediately in the "Before buffer" stage
     */
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 20 elements = everything from upstream
        |After  buffer must be at 5 = 5 (requested)
      """.stripMargin)

    sinkSubscription.request(5)
    Thread.sleep(100)

    println(
      """
        |After buffer must have:
        |  processed from 6 to 9,
        |  discarded 10 to 19, due to OiverflowStrategy.dropTail
        |  and processed the last remaining 20
      """.stripMargin)
  }

  def bufferTestDropBuffer(): Unit ={
    /**
     * From the API Comment about the overflow strategy:
     *   Backpressure - backpressures when buffer is full
     *   DropHead, DropTail, DropBuffer - never backpressures
     *   Fail - fails the stream if buffer gets full
     *
     * The buffer() in the middle "queues up" elements up to 5
     * but no backpressure to upstream ... (continue)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer ${x}"); x})
      .buffer(5, OverflowStrategy.dropBuffer)
      .map(x => {println(s"After  buffer ${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()
    
    sinkSubscription.request(5)
    /**
     *  ... (continued) no backpressure to upstream, hence all the 20 elements
     *  are consumed immediately in the "Before buffer" stage
     */
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 20 = everything from upstream
        |After  buffer must be at 5 = 5 (requested)
      """.stripMargin)

    sinkSubscription.request(5)
    Thread.sleep(100)

    println(
      """
        |After buffer must have processed from 16 to 20, because
        |  elements 6 to 10 filled up the buffer and the dropped entirely,
        |  elements 11 to 15 also filled up the buffer and dropped
      """.stripMargin)
  }

  def bufferTestDropNew(): Unit ={
    /**
     * From the API Comment about the overflow strategy:
     *   Backpressure - backpressures when buffer is full
     *   DropHead, DropTail, DropBuffer - never backpressures
     *   Fail - fails the stream if buffer gets full
     *
     * The buffer() in the middle "queues up" elements up to 5
     * but no backpressure to upstream ... (continue)
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer${x}"); x})
      .buffer(5, OverflowStrategy.dropNew)
      .map(x => {println(s"After buffer${x}"); x})
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(5)
    /**
     *  ... (continued) no backpressure to upstream, hence all the 20 elements
     *  are consumed immediately in the "Before buffer" stage
     */
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println(
      """
        |Before buffer must be at 20 = everything from upstream
        |After  buffer must be at 5 = 5 (requested)
      """.stripMargin)

    sinkSubscription.request(5)
    Thread.sleep(100)

    println(
      """
        |After buffer must have processed from 6 to 10, because
        |  elements 11 to 20 were "New" elements dropped in the buffer, after it was full
      """.stripMargin)
  }

  def bufferTestFail(): Unit ={
    /**
     * The buffer() in the middle "queues up" elements up to 5
     * the buffer size > 5, it backpressures upstream (Before Buffer)
     */
    val ((sourcePublisher, fut), sinkPublisher) = TestSource.probe[Int]
      .map(x => {println(s"Before buffer${x}"); x})
      .buffer(5, OverflowStrategy.fail)
      .map(x => {println(s"After buffer${x}"); x})
      .watchTermination()(Keep.both)
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    val sinkSubscriber = TestSubscriber.manualProbe[Int]()
    sinkPublisher.subscribe(sinkSubscriber)
    val sinkSubscription = sinkSubscriber.expectSubscription()

    sinkSubscription.request(5)
    for(i <- 1 to 20)
      sourcePublisher.sendNext(i)
    Thread.sleep(100)

    println("Before buffer must be at 11 elements = 10 successfully processed, and 1 that is going to fail")
    println("After  buffer must be at 5 = 5 (requested)")

    import scala.concurrent.ExecutionContext.Implicits.global
    fut.onComplete{
      case Success(x) => println("Unexpected: Successfully completed the stream")
      case Failure(x) => println(s"Expected: the stream failed ${x}") // akka.stream.BufferOverflowException: Buffer overflow (max capacity was: 5)!
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("bufferTestBackPressure")(bufferTestBackPressure)
      Wrapper("bufferTestDropHead")(bufferTestDropHead)
      Wrapper("bufferTestDropTail")(bufferTestDropTail)
      Wrapper("bufferTestDropBuffer")(bufferTestDropBuffer)
      Wrapper("bufferTestDropNew")(bufferTestDropNew)
      Wrapper("bufferTestFail")(bufferTestFail)
    }
    finally{
      system.terminate()
    }
  }
}
