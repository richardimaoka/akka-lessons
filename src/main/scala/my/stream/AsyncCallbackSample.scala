package my.stream

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.stage.{AsyncCallback, _}
import akka.stream.testkit.TestSubscriber
import akka.stream.{Attributes, _}
import my.wrapper.Wrapper

import scala.concurrent.Await
import scala.concurrent.duration._

class AsyncCallbackContainer[T] {
  var callback: AsyncCallback[T] = _
  def onEvent(s: T): Unit = {
    println(s"onEvent: ${s}")
  }
}

final case class MyMap[In, Out](asyncContainer: AsyncCallbackContainer[Any], f: In ⇒ Out) extends GraphStage[FlowShape[In, Out]] {
  val in = Inlet[In]("MyMap.in")
  val out = Outlet[Out]("MyMap.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("MyMap")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {

      override def preStart(): Unit = {
        println("initializing callback from preStart()")
        asyncContainer.callback = getAsyncCallback(t => asyncContainer.onEvent(t))
      }

      override def onPush(): Unit = {
        push(out, f(grab(in)))
      }

      override def onPull(): Unit = pull(in)

      setHandlers(in, out, this)
    }
}


final case class PullOnAsyncCallback[T](asyncContainer: AsyncCallbackContainer[Int]) extends GraphStage[FlowShape[T, T]] {
  val in  = Inlet[T]("PullOnAsyncCallback.in")
  val out = Outlet[T]("PullOnAsyncCallback.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("PullOnAsyncCallback")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {

      var downstreamDemand = 0
      var buffer: T = _
      var isBufferFull: Boolean = false

      override def preStart(): Unit = {
        println("initializing callback from preStart()")
        asyncContainer.callback = getAsyncCallback(onCallbackInvokation)
      }

      override def onPush(): Unit = {
        val elem = grab(in)
        println(s"onPush(${elem})")

        if(downstreamDemand > 0){
          println(s"onPush ${elem}, with remaining downstreamDemands = ${downstreamDemand}")
          push(out, elem)
          downstreamDemand -= 1
        }
        else if(buffer != null) {
          throw new Exception("When buffer is full (only 1 element allowed) no mor element can be pushed!!!")
        }
        else{
          buffer = elem
          isBufferFull = true
        }
      }

      override def onPull(): Unit = {
        if(buffer != null){
          push(out, buffer)
          isBufferFull = false
        }
        else
          downstreamDemand += 1
      }

      def onCallbackInvokation(i: Int): Unit ={
        for(j <- 1 to i){
          println("pulling!")
          pull(in)
        }
      }

      setHandlers(in, out, this)
    }
}


final case class CompleteOnAsyncCallback[T](asyncContainer: AsyncCallbackContainer[String]) extends GraphStage[FlowShape[T, T]] {
  val in  = Inlet[T]("CompleteOnAsyncCallback.in")
  val out = Outlet[T]("CompleteOnAsyncCallback.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("CompleteOnAsyncCallback")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {

      var downstreamDemand = 0

      override def preStart(): Unit = {
        println("initializing callback from preStart()")
        asyncContainer.callback = getAsyncCallback(onCallbackInvokation)
      }

      override def onPush(): Unit = {
        val elem = grab(in)
        println(s"onPush ${elem}")
        push(out, elem)
      }

      override def onPull(): Unit = {
        pull(in)
      }

      def onCallbackInvokation(s: String): Unit ={
          println("completing!!")
          completeStage()
      }

      setHandlers(in, out, this)
    }
}


object AsyncCallbackSample {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit = {
    val asyncContainer = new AsyncCallbackContainer[Any]
    val fut = Source(1 to 10)
      .via(new MyMap(asyncContainer, x => 10*x))
      .watchTermination()(Keep.right)
      .to(Sink.ignore)
      .run()

    Thread.sleep(100)

    val callbackReference = asyncContainer.callback
    println(s"callbackReference = ${callbackReference}")

    callbackReference.invoke("hello")

    val result = Await.result(fut, 1 second)
    println(s"result = ${result}")
  }

  def testPullOnAsyncCallback(): Unit = {
    val asyncContainer = new AsyncCallbackContainer[Int]

    val fut = Source(1 to 10)
      .via(new MyCounter[Int]("upstream"))
      .via(new PullOnAsyncCallback(asyncContainer))
      .via(new MyCounter[Int]("downstream"))
      .watchTermination()(Keep.right)
      .to(Sink.ignore)
      .run()

    Thread.sleep(100)

    val callbackReference = asyncContainer.callback
    println(s"callbackReference = ${callbackReference}")

    callbackReference.invoke(1)
    Thread.sleep(50)

    callbackReference.invoke(1)
    Thread.sleep(50)

    callbackReference.invoke(1)
    Thread.sleep(50)
  }

  def testPullOnAsyncCallback2(): Unit = {
    val asyncContainer = new AsyncCallbackContainer[Int]
    val sinkSubscriber = TestSubscriber.manualProbe[Int]()

    val fut = Source(1 to 10)
      .via(new MyCounter[Int]("upstream"))
      .via(new PullOnAsyncCallback(asyncContainer))
      .via(new MyCounter[Int]("downstream"))
      .watchTermination()(Keep.right)
      .to(Sink.fromSubscriber(sinkSubscriber))
      .run()

    val sinkSubscription = sinkSubscriber.expectSubscription()

    val callbackReference = asyncContainer.callback
    println(s"callbackReference = ${callbackReference}")

    sinkSubscription.request(1)
    asyncContainer.callback.invoke(1)
    Thread.sleep(50)

    sinkSubscription.request(1)
    asyncContainer.callback.invoke(1)
    Thread.sleep(50)

    asyncContainer.callback.invoke(1)
    Thread.sleep(50)

    println("***** up to here downstream must not have recognized 3rd element*****")
    sinkSubscription.request(1)
  }

  def testCompleteOnAsyncCallback(): Unit = {
    val asyncContainer = new AsyncCallbackContainer[String]
    val sinkSubscriber = TestSubscriber.manualProbe[Int]()

    val fut = Source(1 to 10)
      .via(new MyCounter[Int]("upstream"))
      .via(new CompleteOnAsyncCallback(asyncContainer))
      .via(new MyCounter[Int]("downstream"))
      .watchTermination()(Keep.right)
      .to(Sink.fromSubscriber(sinkSubscriber))
      .run()

    val sinkSubscription = sinkSubscriber.expectSubscription()
    Thread.sleep(100)

    val callbackReference = asyncContainer.callback
    println(s"callbackReference = ${callbackReference}")

    sinkSubscription.request(1)
    try{
      val result1 = Await.result(fut, 200 milliseconds)
      println(s"result1 = ${result1}")
    }
    catch {
      case e: Exception => println(e)
    }

    asyncContainer.callback.invoke("complete it")
    val result2 = Await.result(fut, 200 milliseconds)
    println(s"result1 = ${result2}")
  }


  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
      Wrapper("testPullOnAsyncCallback")(testPullOnAsyncCallback)
      Wrapper("testPullOnAsyncCallback2")(testPullOnAsyncCallback2)
      Wrapper("testCompleteOnAsyncCallback")(testCompleteOnAsyncCallback)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }
  }
}
