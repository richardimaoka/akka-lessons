package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Broadcast, Concat, Merge, Source}
import my.wrapper.Wrapper


object SourceCombineTest {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def compareCombineWithConcat(): Unit ={
    val s1 = Source(1 to 5)
    val s2 = Source(6 to 10)

    println("Below is combine")
    Source.combine(s1, s2)(i => Merge(i)).runForeach(println(_))
    Thread.sleep(100)

    println("Below is concat")
    s1.concat(s2).runForeach(println(_))
  }

  def compareCombineWithConcat2(): Unit ={
    val s1 = Source(1 to 5)
    val s2 = Source(6 to 10)

    println("Below is combine")
    Source.combine(s1, s2)(i => Concat(i)).runForeach(println(_))
    Thread.sleep(100)

    println("Below is concat")
    s1.concat(s2).runForeach(println(_))
  }

  def testCombineMemberMethod(): Unit ={
    val s1 = Source(1 to 5)
    val s2 = Source(6 to 10)
    val s3 = Source(11 to 15)

    s1.combine(s2, s3)(i => Merge(i)).runForeach(println(_))
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("compareCombineWithConcat")(compareCombineWithConcat)
      Wrapper("compareCombineWithConcat2")(compareCombineWithConcat2)
      Wrapper("testCombineMemberMethod")(testCombineMemberMethod)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
