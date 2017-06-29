package my.stream

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, RunnableGraph, Sink, Source}
import my.wrapper.Wrapper

object GraphDSLbuilderTest {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
    val g = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      import GraphDSL.Implicits._
      val in = Source(1 to 10)
      val out = Sink.ignore

      val bcast = builder.add(Broadcast[Int](2))
      val merge = builder.add(Merge[Int](2))

      val f1, f2, f3, f4 = Flow[Int].map(_ + 10)

      in ~> f1 ~> bcast ~> f2 ~> merge ~> f3 ~> out
      bcast ~> f4 ~> merge
      ClosedShape
    })

    g.run()
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
