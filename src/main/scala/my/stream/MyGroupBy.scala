package my.stream

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, RunnableGraph, Sink, Source, SubFlow}
import akka.stream.testkit.scaladsl.TestSource
import akka.stream.testkit.{TestPublisher, TestSubscriber}
import my.wrapper.Wrapper

import scala.collection.immutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

/**
  * Materialization
  */

object MyGroupBy {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  case class Topic(name: String)
  type Message = String

  implicit class Lift[M](val f: SubFlow[Int, M, Source[Int, M]#Repr, RunnableGraph[M]]) extends AnyVal {
    def lift(key: Int ⇒ Int) = f.prefixAndTail(1).map(p ⇒ key(p._1.head) → (Source.single(p._1.head) ++ p._2)).concatSubstreams
  }

  /**
   * See how Source.mapConcat() works .... flattening iterators inside
   */
  def mapConcat(): Unit = {
    val original = Source(1 to 4)
    println("Source.map")
    original.map(x => List(x, x+100, x+200)).runForeach(println(_))
    Thread.sleep(50)

    println("Source.mapConcat")
    original.mapConcat(x => List(x, x+100, x+200)).runForeach(println(_))
  }

  def publisherGroupBy(): Unit = {
    /**
     * Observe the sequence of printed out elements are not in the increasing order,
     * as substreams can run asynchronously to each other
     */
    val publisher = TestSource.probe[Int]
      .groupBy(8, _ % 8)
      .to(Sink.foreach(x => print(s"${x}, ")))
      .run()

    for(i <- 1 to 20)
      publisher.sendNext(i)
    Thread.sleep(100)
    println("")

    /**
     * grouped(10) in the middle chunks up elemtns: Int into
     * multiple Seq[Int] with size = 10
     */
    val (sourcePublisher, sinkPublisher) = TestSource.probe[Int]
      .groupBy(2, _ % 2)
      .grouped(10)
      .concatSubstreams
      .toMat(Sink.asPublisher(false))(Keep.both)
      .run()

    /**
     * subscriber of Seq[Int] as grouped(10) will emit Seq[Int],
     * if there are 10 elements or upstream completes
     */
    val sinkSubscriber = TestSubscriber.manualProbe[Seq[Int]]()
    sinkPublisher.subscribe(sinkSubscriber)
    val subscription = sinkSubscriber.expectSubscription()

    for(i <- 1 to 11)
      sourcePublisher.sendNext(i)

    /**
     * Due to grouped(10) in the middle, elements are not emitted to the sink
     * as it does not exceed the 10 elements
     */
    subscription.request(50)
    sinkSubscriber.expectNoMsg(200 milliseconds)

    subscription.request(6)
    sinkSubscriber.expectNoMsg(200 milliseconds)

    /**
     * This should let the grouped(10) stream emit the first Seq[Int] with the size 10
     */
//    subscription.request(6)
//    sourcePublisher.sendComplete()

  }

  /**
   * groupBy() closed with runForeach sink works like a normal stream
   * (i.e.) not a good example to demonstrate groupBy()
   */
  def simpleButNotADecentExample(): Unit ={
    val topicMapperr: Int => String = (msg: Int) => if( msg.equals(1) ) "1" else "10"
    val listSource  = Source(1 to 5)

    println("listSource.groupBy(1, topicMapperr)")
    println(listSource.groupBy(1, topicMapperr))
    Thread.sleep(50)

    println("listSource.runForeach(x => println(x))")
    listSource.runForeach(x => println(x))
    /**
     * From the API doc
     * Substream mode is exited either by closing the substream (i.e. connecting it to a [[Sink]]) <- this is happening in this example
     * (or by merging the substreams back together)
     */
    Thread.sleep(50)
  }

  def recipeMultiGroupBy(): Unit = {
    /***********************************/
    /*     From RecipeMultiGroupBy     */
    /***********************************/
    val elemsBase = List("1: a", "1: b", "all: c", "all: d", "1: e")
    /**
     *  elems: Source[iterable, NotUsed] =
     *    "1: a" ->
     *    "1: b" ->
     *    ...
     */
    val elems = Source(elemsBase)

    /**
     * elems.foreach(extractTopics(_))
     *    List(Topic(1)) ->
     *    List(Topic(1)) ->
     *    List(Topic(1), Topic(2)) -> //extracted 2 topics for the same element, will be flattened by mapConcat
     *    List(Topic(1), Topic(2)) ->
     *    List(Topic(1)) ->
     */
    val extractTopics = { msg: Message =>
      if (msg.startsWith("1")) List(Topic("1"))
      else List(Topic("1"), Topic("2"))
    }
    println("elemsBase.foreach(msg => println(extractTopics(msg)))")
    elemsBase.foreach(msg => println(extractTopics(msg)))
    Thread.sleep(50)

    //Hmm, topicMapper === extractTopics !?
    val topicMapper: (Message) => immutable.Seq[Topic] = extractTopics
    println("elemsBase.foreach(msg => println(topicMapper(msg)))")
    elemsBase.foreach(msg => println(topicMapper(msg)))
    Thread.sleep(50)

    /**
     *  messageAndTopic: Source[(Message, Topic), NotUsed]
     *   ("1: a",   Topic(1)) ->
     *   ("1: b",   Topic(1)) ->
     *   ("all: c", Topic(1)) ->
     *   ("all: c", Topic(2)) ->  //"all: c" appears twice, for a different topic, List(Topic(1), Topic(2)) flattened by mapConcat
     *   ("all: d", Topic(1)) ->
     *   ("all: d", Topic(2)) ->  //"all: d" appears twice, for a different topic, List(Topic(1), Topic(2)) flattened by mapConcat
     *   ("1: e",   Topic(1)) ->
     */
    val messageAndTopic: Source[(Message, Topic), NotUsed] = elems.mapConcat { msg: Message =>
      val topicsForMessage = topicMapper(msg)
      // Create a (Msg, Topic) pair for each of the topics
      // the message belongs to
      topicsForMessage.map(msg -> _)
    }
    println("messageAndTopic.runForeach(println(_))")
    messageAndTopic.runForeach(println(_))
    Thread.sleep(50)

    val multiGroups = messageAndTopic
      // As messageAndTopic: Source[(Message, Topic), NotUsed],
      // groupBy pushes through elements : (Message, Topic)
      .groupBy(2, _._2)  //_._2 means that the topic is the second element of tuple (Message, Topic)
      .map {
        case (msg, topic) =>
          // do what needs to be done
          println(s"split into: msg = ${msg}, topic=${topic}")
          (msg, topic)
      }
    println("multiGroups.map(println(_))")
    /**
     * From the API doc
     * Substream mode is exited either by closing the substream (i.e. connecting it to a [[Sink]]) <- this is happening in this example
     */
    multiGroups.to(Sink.foreach(println(_)))
    Thread.sleep(50)

    val result = multiGroups
      .grouped(10)
      .mergeSubstreams
      .map(g => g.head._2.name + g.map(_._1).mkString("[", ", ", "]"))
      .limit(10)
      .runWith(Sink.seq)

    println("Await.result(result, 3.seconds).toSet")
    println(Await.result(result, 3.seconds).toSet)

    //Scope to introduce the global execution context
    {
      import scala.concurrent.ExecutionContext.Implicits.global
      result.onComplete({
        case Success(v) => println("v.getClass = " + v.getClass)
      })
    }
    Thread.sleep(50)
  }

  def flowGroupBySpec(): Unit = {
    /*************************************************************/
    //                    From FlowGroupBySpec
    /*************************************************************/
    //It's a complete stream already run, but not any element demanded yet by the publisher
    val publisher = Source(1 to 15).map(x => {println(s"demanded in original stream: ${x}"); x}).runWith(Sink.asPublisher(false))
    println(s"pub.getClass = ${publisher.getClass}, ${publisher}")


    //*********************************************************
    //*********************************************************
    // BUG !? !? !?
    // For some reason if you remove this piece of code,
    // all the 15 elements are demanded by groupPublisher
    // even if the grouped stream is not demanded!??? or `lift` below demands elements?
    //*********************************************************
    //*********************************************************
//    val subscriber = TestSubscriber.manualProbe[Int]()
//    publisher.subscribe(subscriber)
//    val subscription = subscriber.expectSubscription()
//    subscription.request(10)
//    Thread.sleep(50)

    //Creating a stream `Source`-ed from the above publisher
    val max = 5
    val groupCount  = 2
    val groupPublisher = Source.fromPublisher(publisher)
      .groupBy(max, _ % groupCount)
      .lift(_ % groupCount)
      .map(x => {println(s"demanded in grouped Stream: ${x}");x})
      .runWith(Sink.asPublisher(false))
    println(s"groupPublisher.getClass = ${groupPublisher.getClass}, ${groupPublisher}")

    val groupSubscriber = TestSubscriber.manualProbe[(Int, Source[Int, _])]()
    groupPublisher.subscribe(groupSubscriber)
    val groupSubscription = groupSubscriber.expectSubscription()
    groupSubscription.request(5)


//    Source(0 to 16).groupBy(1, elem => "all").concatSubstreams.runForeach(println(_))
//    Thread.sleep(50)
//
//    val p = Source(0 to 16).groupBy(1, elem => "all").to(Sink.asPublisher(false)).run()
//    println(s"p.getClass = ${p.getClass}")
//    //      val probe2 = TestSubscriber.manualProbe[Int]()
//    //      publisher.subscribe(probe)
//    //      val subscription2 = probe.expectSubscription()

    Thread.sleep(50)
  }

  def substreamSubscriptionTimeoutSpec(): Unit = {
    /*************************************************************/
    //           From SubstreamSubscriptionTimeoutSpec
    /*************************************************************/
    val publisherProbe = TestPublisher.probe[Int]()
    val subscriber = TestSubscriber.manualProbe[(Int, Source[Int, _])]()
    val publisher = Source.fromPublisher(publisherProbe).map(x=>{println(s"mapped! ${x}"); x}).groupBy(2, _ % 2).lift(_ % 2).runWith(Sink.fromSubscriber(subscriber))

    val downstreamSubscription = subscriber.expectSubscription()
    downstreamSubscription.request(5)

    publisherProbe.sendNext(1)
    publisherProbe.sendNext(2)
    publisherProbe.sendNext(3)
    publisherProbe.sendComplete()

    println(subscriber.expectNext())
    println(subscriber.expectNext())

    Thread.sleep(50)
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("mapConcat")(mapConcat)
      Wrapper("publisherGroupBy")(publisherGroupBy)
      Wrapper("simpleButNotADecentExample")(simpleButNotADecentExample)
      Wrapper("recipeMultiGroupBy")(recipeMultiGroupBy)
//      flowGroupBySpec()
//      println("\n\n")
//      substreamSubscriptionTimeoutSpec()
    }
    finally{
      system.terminate()
    }
  }
}
