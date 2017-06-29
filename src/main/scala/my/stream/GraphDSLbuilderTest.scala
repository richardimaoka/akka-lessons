package my.stream

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, Merge, RunnableGraph, Sink, Source, Zip, ZipWith}
import akka.stream.testkit.TestSubscriber
import my.wrapper.Wrapper

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

object GraphDSLbuilderTest {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
    /**
     * implicit builder: GraphDSL.Builder[NotUsed] is used by import GraphDSL.Implicits._
     * which has methods like ~> ... jump to the definition of ~> and see
     */
    val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._
      val in = Source(1 to 10)
      val out = Sink.foreach[Int](println(_))

      val bcast = builder.add(Broadcast[Int](2))
      val merge = builder.add(Merge[Int](2))

      val f1, f2, f3, f4 = Flow[Int].map(_ + 10)

      /**
       * in ~> f1 ~> bcast ~> f2 ~> merge ~> f3 ~> out
       *             bcast ~> f4 ~> merge
       */
      in ~> f1 ~> bcast ~> f2 ~> merge ~> f3 ~> out
      bcast ~> f4 ~> merge
      ClosedShape
    })

    g.run()
  }

  def testPartial(): Unit = {
    val pickMaxOfThree = GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      val zip1 = b.add(ZipWith[Int, Int, Int](math.max _))
      val zip2 = b.add(ZipWith[Int, Int, Int](math.max _))

      /**
       *  -->  in0             ---> in0
       *             zip1.out /         zip2.out ---->
       *  -->  in1             ---> in1
       *                      /
       *  -------------------/
       */
      zip1.out ~> zip2.in0

      UniformFanInShape(zip2.out, zip1.in0, zip1.in1, zip2.in1)
    }

    val resultSink = Sink.head[Int]

    val g = RunnableGraph.fromGraph(GraphDSL.create(resultSink) { implicit b => sink =>
      import GraphDSL.Implicits._

      // importing the partial graph will return its shape (inlets & outlets)
      val pm3 = b.add(pickMaxOfThree)

      Source.single(1) ~> pm3.in(0)
      Source.single(2) ~> pm3.in(1)
      Source.single(3) ~> pm3.in(2)
      pm3.out ~> sink.in
      ClosedShape
    })

    val max: Future[Int] = g.run()
    val result = Await.result(max, 300.millis)
    println(s"finished with ${result}")
  }

  def testMap(): Unit = {
    val m = GraphDSL.create() { implicit b =>
      val map = b.add(Flow[Int].map(x => x*5))
      FlowShape[Int, Int](map.in, map.out)
    }

    Source(1 to 5).via(m).runForeach(println(_))
  }

  def testConstruct(): Unit = {
    val pairs = Source.fromGraph(GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._

      // prepare graph elements
      val zip = b.add(Zip[Int, Int]())
      def ints = Source.fromIterator(() => Iterator.from(1))

      // connect the graph
      ints.filter(_ % 2 != 0) ~> zip.in0
      ints.filter(_ % 2 == 0) ~> zip.in1

      // expose port
      SourceShape(zip.out)
    })

    val firstPair: Future[(Int, Int)] = pairs.runWith(Sink.head)
    val result = Await.result(firstPair, 1 second)
    println(s"result = ${result}")
  }

  def testConcat(): Unit = {
    val promise = Promise[Int]()
    val subscriber = TestSubscriber.manualProbe[Int]()

    RunnableGraph.fromGraph(GraphDSL.create() { implicit b ⇒
      import GraphDSL.Implicits._

      val concat = b add Concat[Int]()
      Source(List(1, 2, 3)) ~> concat.in(0)
      Source.fromFuture(promise.future) ~> concat.in(1)
      concat.out ~> Sink.fromSubscriber(subscriber)
      ClosedShape
    }).run()

    val subscription = subscriber.expectSubscription()
    subscription.request(4)

    /**
     * expectNext() throws an AssertionError if no element received
     */
    subscriber.expectNext()
    subscriber.expectNext()
    subscriber.expectNext()

    promise.success(5)
    subscriber.expectNext()
    println("ok - if you reached this point, no exception thrown up to here")
  }

  def testSimpleBuilder(): Unit = {
    val sourceOne = Source(List(1))
    val sourceTwo = Source(List(2))
    val merged = Source.combine(sourceOne, sourceTwo)(Merge(_))

    val mergedResult: Future[Int] = merged.runWith(Sink.fold(0)(_ + _))
    val result = Await.result(mergedResult, 1 second)
    println(s"result = ${result}")
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
      Wrapper("testPartial")(testPartial)
      Wrapper("testMap")(testMap)
      Wrapper("testConstruct")(testConstruct)
      Wrapper("testConcat")(testConcat)
      Wrapper("testSimpleBuilder")(testSimpleBuilder)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
