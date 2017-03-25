import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

/**
  * Sink,
  * blueprint .... blueprint is not an Akka Stream terminology thought (it should be called Graph but do we introduce it here?)
  * Run
  */
object My3rdStream {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val sourceFromRange = Source(1 to 5)
    val sinkForEach = Sink.foreach{ i: Int => println(i) } //[Int] is needed

    sourceFromRange.runWith(sinkForEach)

    val r = sourceFromRange.to(sinkForEach)
    r.run()

    //beginner tips: {} == (), nicer syntax for passing closure

    Thread.sleep(1000)
    system.terminate()
  }
}