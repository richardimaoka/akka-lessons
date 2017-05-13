import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}

/**
  * Materialization
  */
object My4thStream {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val sourceFromRange = Source(1 to 5)
    val sinkForEach = Sink.foreach{ i: Int => println(i) } //[Int] is needed

    //print List() = Nil
    val materialized = sourceFromRange.runWith(sinkForEach)
    println(s"materialized value = ${materialized}")

    Thread.sleep(50)
    println(s"materialized value = ${materialized}")

    val m1 = sourceFromRange runWith( Sink.head )
    Thread.sleep(50)
    println(m1)

    val m2 = sourceFromRange runWith( Sink.headOption )
    Thread.sleep(50)
    println(m2)

    val m3 = sourceFromRange runWith( Sink.last )
    Thread.sleep(50)
    println(m3)

    val m4 = sourceFromRange runWith( Sink.lastOption )
    Thread.sleep(50)
    println(m4)

    val m5 = sourceFromRange runWith( Sink.seq )
    Thread.sleep(50)
    println(m5)

    val m6 = sourceFromRange runWith( Sink.ignore )
    Thread.sleep(50)
    println(m6)

    Thread.sleep(1000)
    system.terminate()
  }
}