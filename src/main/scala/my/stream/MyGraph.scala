package my.stream

import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream._
import akka.stream.scaladsl.Source
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import my.wrapper.Wrapper

/**
  * Materialization
  */
class MyGraph[T] extends GraphStage[FlowShape[T,T]]{
  val in = Inlet[T](Logging.simpleName(this) + ".in")
  val out = Outlet[T](Logging.simpleName(this) + ".out")
  override val shape = FlowShape(in, out)

  override def initialAttributes = Attributes.name("mygraph")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) with InHandler with OutHandler {
    def onPush(): Unit = {
      val elem = grab(in)
      println(s"mygraph onPush(), grabbed elem: ${elem}")
      push(out, elem)
    }
    def onPull(): Unit = {
      println("mygraph onPull()")
      pull(in)
    }

    setHandler(in, this)
    setHandler(out, this)
  }

  override def toString = "Identity"
}

/**
 * Push the same element twice without the downstream demand
 * -> it will throw an exception
 *
 *
 * [ERROR] [06/23/2017 14:05:23.532] [default-akka.actor.default-dispatcher-2]
 *   [akka://default/user/StreamSupervisor-0/flow-0-0-ignoreSink]
 *   Error in stage [Identity]: requirement failed: Cannot push port (MyGraph.out(1409433252)) twice
 */
class MyFailingGraph[T] extends GraphStage[FlowShape[T,T]]{
  val in = Inlet[T](Logging.simpleName(this) + ".in")
  val out = Outlet[T](Logging.simpleName(this) + ".out")
  override val shape = FlowShape(in, out)

  override def initialAttributes = Attributes.name("mygraph")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) with InHandler with OutHandler {
    def onPush(): Unit = {
      val elem = grab(in)
      println(s"mygraph onPush(), grabbed elem: ${elem}")
      push(out, elem)
      push(out, elem)
    }
    def onPull(): Unit = {
      println("mygraph onPull()")
      pull(in)
    }

    setHandler(in, this)
    setHandler(out, this)
  }

  override def toString = "Identity"
}

object MyGraph {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  def test(): Unit ={
    Source(1 to 10).via(new MyGraph[Int]).via(new MyGraph[Int]).runForeach(println(_))
  }

  def testFail(): Unit ={
    Source(1 to 10).via(new MyFailingGraph[Int]).runForeach(println(_))
  }

  def main(args: Array[String]): Unit = {
    try {
      Wrapper("test")(test)
      Wrapper("testFail")(testFail)
    }
    finally{
      system.terminate()
    }
  }
}
