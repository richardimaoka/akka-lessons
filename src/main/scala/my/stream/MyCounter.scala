package my.stream

import akka.stream.{Attributes, FlowShape, Inlet, Outlet}
import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}

class MyCounter[T](prefix: String) extends GraphStage[FlowShape[T, T]] {
  val in = Inlet[T]("MyCounter.in")
  val out = Outlet[T]("MyCounter.out")
  override val shape = FlowShape(in, out)

  override def initialAttributes: Attributes = Attributes.name("MyCounter")

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) with InHandler with OutHandler {
      override def onPush(): Unit = {
        val elem = grab(in)
        println(s"${prefix} pushed: ${elem}")
        push(out, elem)
      }

      override def onPull(): Unit = {
        println(s"${prefix} pulled")
        pull(in)
      }

      setHandlers(in, out, this)
    }
}

