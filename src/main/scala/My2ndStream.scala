import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source

object My2ndStream {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    // https://opencredo.com/introduction-to-akka-streams-getting-started/
    //hide signatures until signatures become meaningful
    val sourceFromSingle = Source.single(1)
    sourceFromSingle runForeach{ i => println(i) }

    val sourceFromRange = Source(1 to 5)
    sourceFromRange runForeach{ i => println(i) }

    val sourceFromIterable = Source(List(1,2,3))
    sourceFromIterable runForeach{ i => println(i) }

    Thread.sleep(1000)
    system.terminate()

    //val sourceFromFuture = Source(Future.successful("hello"))
//    val emptySource = Source.empty
//
//    val source: Source[Int, Promise[Option[Int]]] = Source.maybe[Int]
//
//    // A sink that returns the first element of a stream in the returned Future
//    //val sink: Sink[Int, Future[Int]] = Sink.head[Int]
//    val sink = Sink.foreach( println )
//
//    // By default, the materialized value of the leftmost stage is preserved
//    val r1: RunnableGraph[Promise[Option[Int]]] = source.to(sink)
//
//    println(r1.run().success(Some(10)))

  }

}