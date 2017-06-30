package my.stream

import akka.event.Logging
import akka.stream.{FlowShape, Inlet, Outlet}
import akka.stream.stage.GraphStage

abstract class SimpleLinearGraphStage[T] extends GraphStage[FlowShape[T, T]] {
  val in = Inlet[T](Logging.simpleName(this) + ".in")
  val out = Outlet[T](Logging.simpleName(this) + ".out")
  override val shape = FlowShape(in, out)
}
