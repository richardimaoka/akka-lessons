package my.pullreq

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorAttributes, ActorMaterializer, Supervision}

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MapMergeConcatError extends App {
  implicit val system = ActorSystem("Main")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val subFlow = {
    Flow[Int]
      .mapAsyncUnordered(5)(i => Future {
        if (i == 4) sys.error("☠")
        i * 5
      })
      .withAttributes(ActorAttributes.supervisionStrategy(Supervision.stoppingDecider))
      .reduce(_ + _)
  }

  /**
   * [
   *   [1,2],
   *   [3,4,5],
   *   [6]
   * ]
   */
  val subStreamFuture = Source(Seq(Seq(1, 2), Seq(3, 4, 5), Seq(6)))
    .flatMapMerge(5, m => Source.single(m).mapConcat(identity).via(subFlow))
    .withAttributes(ActorAttributes.supervisionStrategy(Supervision.resumingDecider))
    .runWith(Sink.seq)

  val mapAsyncFuture = Source(Seq(Seq(1, 2), Seq(3, 4, 5), Seq(6)))
    .mapAsyncUnordered(5)(m => Source.single(m).mapConcat(identity).via(subFlow).runWith(Sink.head))
    .withAttributes(ActorAttributes.supervisionStrategy(Supervision.resumingDecider))
    .runWith(Sink.seq)

  val f1 = Await.ready(subStreamFuture, 10.seconds)
  val f2 = Await.ready(mapAsyncFuture, 10.seconds)

  println(s"Using flatMapMerge: $f1")
  println(s"Using mapAsyncUnordered: $f2")

  system.terminate()
}
