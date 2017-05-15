package my.stream

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, FlowShape}
import akka.stream.scaladsl.{Flow, GraphDSL, Keep, Sink, Source}

/**
  * Materialization
  */
object My5thStream {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()


    try {
      val ignoreSink = Sink.ignore
      val singleSource = Source.single(1)
      val identityFlow = Flow[Int]
      val singleIdentityFlow = singleSource.via(identityFlow)

      Flow.fromSinkAndSource[Any, Int](ignoreSink, singleIdentityFlow)

      //GraphDSL.create(sink, source_flow){Keep.right}{ implicit b ⇒ (in, out) ⇒ FlowShape(in.in, out.out) }

//      Flow.fromSinkAndSource[Any, Int](
//        Sink.ignore
//        , Source
//          .single(1)
//          .via(Flow[Int])
//      )
    }
    finally{
      system.terminate()
    }
  }
}