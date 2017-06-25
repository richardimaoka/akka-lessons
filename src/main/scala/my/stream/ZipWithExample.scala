package my.stream

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import my.wrapper.Wrapper

class MyCounter[T](prefix: String) extends GraphStage[FlowShape[T, T]] {
  val in = Inlet[T]("MyCounter.in")
  val out = Outlet[T]("MyCounter.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("MyCounter")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {
      override def onPush(): Unit = {
        val elem = grab(in)
        println(s"${prefix} pushed: ${elem}")
        push(out, elem)
      }

      override def onPull(): Unit = {
        println(s"${prefix} pulled")
        pull(in)
      }

      setHandlers(in, out, this)
    }
}

/** `ZipWith` specialized for 2 inputs */
class MyZipWith2[A1, A2, O](val zipper: (A1, A2) ⇒ O) extends GraphStage[FanInShape2[A1, A2, O]] {
  override def initialAttributes = Attributes.name("ZipWith2")
  override val shape: FanInShape2[A1, A2, O] = new FanInShape2[A1, A2, O]("ZipWith2")
  def out: Outlet[O] = shape.out
  val in0: Inlet[A1] = shape.in0
  val in1: Inlet[A2] = shape.in1

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    var pending = 0
    // Without this field the completion signalling would take one extra pull
    var willShutDown = false

    private def pushAll(): Unit = {
      push(out, zipper(grab(in0), grab(in1)))
      if (willShutDown) completeStage()
      else {
        pull(in0)
        pull(in1)
      }
    }

    override def preStart(): Unit = {
      pull(in0)
      pull(in1)
    }

    setHandler(in0, new InHandler {
      override def onPush(): Unit = {
        pending -= 1
        if (pending == 0) pushAll()
      }

      override def onUpstreamFinish(): Unit = {
        if (!isAvailable(in0)) completeStage()
        willShutDown = true
      }

    })
    setHandler(in1, new InHandler {
      override def onPush(): Unit = {
        pending -= 1
        if (pending == 0) pushAll()
      }

      override def onUpstreamFinish(): Unit = {
        if (!isAvailable(in1)) completeStage()
        willShutDown = true
      }

    })

    setHandler(out, new OutHandler {
      override def onPull(): Unit = {
        pending += shape.inlets.size
        if (pending == 0) pushAll()
      }
    })
  }

  override def toString = "ZipWith2"
}

class MyZip[A,B] extends MyZipWith2[A,B,(A,B)](Tuple2.apply){
  override def toString = "MyZip"
}

object ZipWithExample {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  private def zipWithInput[In, Out, _](flow: Flow[In, Out, _]) = {
    Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val broadcast = builder.add(Broadcast[In](2))
      val zip = builder.add(Zip[In, Out]())

      broadcast.out(0) ~> zip.in0
      broadcast.out(1) ~> flow ~> zip.in1

      FlowShape(broadcast.in, zip.out)
    })
  }

  /**
   * This doesn't have the same problem as bcast -> zip
   */
  private def bcastSample[T]= {
    Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val broadcast = builder.add(Broadcast[T](2))
      val merge = builder.add(Merge[T](2))

      broadcast.out(0) ~> merge.in(0)
      broadcast.out(1) ~> merge.in(1)

      FlowShape(broadcast.in, merge.out)
    })
  }

  /**
   * bcast -> zip has a problem where upstream pulls earlier than downstream pull
   */
  private def bcastZip[T]= {
    Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val broadcast = builder.add(Broadcast[T](2))
      val zip = builder.add(Zip[T, T]())

      broadcast.out(0) ~> zip.in0
      broadcast.out(1) ~> zip.in1

      FlowShape(broadcast.in, zip.out)
    })
  }

  def simpleFlow = Flow[String]
    //.map(key => {println(s"simpleFlow: ${Map("key" -> key)}"); key;})
    .map(_ => List("111", "222", "333"))

  def testIssue(): Unit = {
    println("exampleOk.run()")
    exampleOk.run() ; Thread.sleep(500)
    println("exampleFail.run()")
    exampleFail.run() ; Thread.sleep(500)

    def exampleOk = Source(List("AAA", "BBB", "CCC"))
      .via(zipWithInput(simpleFlow))
      // Converting act: List[String] into List[Map[String, String]]
      // where "->" in (prev -> _) is a tuple creation operation
      .mapConcat { case (prev, act) => act.map(prev -> _)}
      .to(Sink.foreach(println))

    def exampleFail = Source(List("AAA", "BBB", "CCC"))
      .via(zipWithInput(simpleFlow.mapConcat(a => a)))
      .to(Sink.foreach(println))
  }

  def myCounterTest(): Unit ={
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream  "))
      .via(Flow[String])
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def investigate(): Unit = {
    println("Listing out the source")
    Source(List("AAA", "BBB", "CCC")).to(Sink.foreach(println)).run()
    Thread.sleep(50)

    println("\nZipWith")
    Source(List("AAA", "BBB", "CCC"))          //Source(iterable) is Source[T, NotUsed]
      .zipWith(Source(List(1,2,3)))(Keep.both) //This Keep.both is not about materialized values, but about Out values
      .to(Sink.foreach(println))
      .run()
    Thread.sleep(50)

    println("\nsimpleFlow")
    Source(List("AAA", "BBB", "CCC"))
      .via(simpleFlow)
      .to(Sink.foreach(println))
      .run()
    Thread.sleep(50)

    println("\nzipWithInput")
    Source(List("AAA", "BBB", "CCC"))
      .via(zipWithInput(Flow[String]))
      .to(Sink.foreach(println))
      .run()
    Thread.sleep(50)

    println("\nzipWithInput(simpleFlow)")
    Source(List("AAA", "BBB", "CCC"))
      .via(zipWithInput(simpleFlow))
      .to(Sink.foreach(println))
      .run()
  }

  def mapConcatTest(): Unit = {
    Source(List(List(1,2,3,4,5), List(1,2,3,4,5), List(1,2,3,4,5)))
      .mapConcat(a => a)
      .runForeach(println(_))
  }

  def simpleFlowTest(): Unit = {
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream  "))
      .via(simpleFlow)
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def simpleFlowZipWithInput(): Unit ={
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream  "))
      .via(zipWithInput(simpleFlow))
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  /**
   * This looks workgin, but due to bcast -> zip, it seems that
   * upstream pulls earlier than downstream ...?
   */
  def simpleFlowZipWithInputMapConcat(): Unit ={
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream1 "))
      .via(zipWithInput(simpleFlow))
      .via(new MyCounter("upstream2 "))
      // def mapConcat[T](f: Out ⇒ immutable.Iterable[T]): Repr[T]
      .mapConcat { case (prev, act) => { // (e.g.)prev = "AAA", act = List("111", "222", "333")
//        println(s"prev ${prev}")
//        println(s"act ${act}")
//        println(s"act.map(prev -> _) ${act.map(prev -> _)}") // -> is used to crate an "Entry" in Map
        act.map(prev -> _)
      }}
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
    Thread.sleep(500)
  }

  def simpleFlowMapConcat(): Unit = {
    Source(List("AAA","BBB","CCC"))
      //def mapConcat[T](f: Out ⇒ immutable.Iterable[T]): Repr[T]
      .via(new MyCounter("upstream  "))
      .via(simpleFlow.mapConcat(a => a))
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def simpleFlowMapConcatZipWithInput(): Unit = {
    Source(List("AAA","BBB","CCC"))
      //def mapConcat[T](f: Out ⇒ immutable.Iterable[T]): Repr[T]
      .via(new MyCounter("upstream  "))
      .via(zipWithInput(simpleFlow.mapConcat(a => a)))
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def zipWithInputPlainInput(): Unit = {
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream  "))
      .via(zipWithInput(Flow[String]))
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def bcastTest(): Unit = {
    Source(List("AAA", "BBB", "CCC"))
      .via(new MyCounter("upstream  "))
      .via(bcastSample[String])
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def bcastZipTest(): Unit = {
    Source(List("AAA","BBB","CCC"))
      .via(new MyCounter("upstream  "))
      .via(bcastZip[String])
      .via(new MyCounter("downstream"))
      .runForeach(println(_))
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("myCounterTest")(myCounterTest)
      Wrapper("testsIssue")(testIssue)
      Wrapper("investigate")(investigate)
      Wrapper("mapConcatTest")(mapConcatTest)
      Wrapper("simpleFlowTest")(simpleFlowTest)
      Wrapper("simpleFlowZipWithInput")(simpleFlowZipWithInput)
      Wrapper("simpleFlowZipWithInputMapConcat")(simpleFlowZipWithInputMapConcat)
      Wrapper("simpleFlowMapConcat")(simpleFlowMapConcat)
      Wrapper("simpleFlowMapConcatZipWithInput")(simpleFlowMapConcatZipWithInput)
      Wrapper("zipWithInputPlainInput")(zipWithInputPlainInput)
      Wrapper("bcastTest")(bcastTest)
      Wrapper("bcastZipTest")(bcastZipTest)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
