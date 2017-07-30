package my.stream

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

object FlatMapMergeApp {
  implicit val system = ActorSystem("Main")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  def case1(): Unit ={
    val async = Flow[Int].map(_ * 2).async
    val fut = Source(0 to 9)
      .map(_ * 10)
      /**
       * def flatMapMerge[T, M](breadth: Int, f: Out ⇒ Graph[SourceShape[T], M]): Repr[T]
       *   = map(f).via(new FlattenMerge[T, M](breadth))
       *
       * (i.e.) firstly f is applied via `map`
       * So what FlattenMerge receives (gets pushed) is Source to which `f` is already applied
       */
      .flatMapMerge(5, i ⇒ Source(i to (i + 9)).via(async))
      .grouped(100)
      .runWith(Sink.head)

    val result = Await.result(fut, 1 second)
    println(result)
  }

  def case2(): Unit = {
    def src10(i: Int) = Source(i until (i + 10))

    val toSeq = Flow[Int].grouped(1000).toMat(Sink.head)(Keep.right)
    val toSet = toSeq.mapMaterializedValue(_.map(_.toSet))

    val fut = Source(List(src10(0), src10(10), src10(20), src10(30)))
      .flatMapMerge(4, identity)
      .runWith(toSet)

    val result = Await.result(fut, 1 second)
    println(result)
  }

  def main(args: Array[String]): Unit = {
    try{
      Wrapper("case1")(case1)
      Wrapper("case2")(case2)
    } finally {
      system.terminate()
    }
  }
}
