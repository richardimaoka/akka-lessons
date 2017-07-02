package akka.contrib.d3.my

import akka.Done
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

abstract class StartupTaskProvider {
  def startupTask(
                   name:                String,
                   task:                () ⇒ Future[Done],
                   timeout:             FiniteDuration,
                   minBackoff:          FiniteDuration,
                   maxBackoff:          FiniteDuration,
                   randomBackoffFactor: Double
                 ): StartupTask
}
