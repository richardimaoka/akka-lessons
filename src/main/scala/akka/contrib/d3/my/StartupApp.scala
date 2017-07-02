package akka.contrib.d3.my

import akka.Done
import akka.actor.ActorSystem

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class StartupApp(system: ActorSystem) {

  implicit val ec = system.dispatcher

  /**
   * Mapping from ActorSystem -> Extension
   * Akka extension is instantiated for an ActorSystem
   */
  val startupTasks = StartupTasks(system)
  implicit val timeout = akka.util.Timeout(10.seconds)

  def run(): Future[Done] = {
    println(
      """
        |***************************************************
        | Creating StartupTask
        |***************************************************
      """.stripMargin
    )
    val startupTask = startupTasks.createTask(
      name       = "boom",
      task       = () => Future { Thread.sleep(2000); println("finished task"); akka.Done }, //() ⇒ Future[Done]
      timeout    = 5.seconds,
      minBackoff = 5.seconds,
      maxBackoff = 30.seconds,
      randomBackoffFactor = 0.5
    )
    Thread.sleep(3000)

    println(
      """
        |*****************************************************************
        | Executing StartupTask multiple times
        | At this point StartupTaskActor already in the `Executed` state
        |
        | So the `Execute` message sent in execute() will be handled by
        | StartupActor's `executed: Receive` handler
        |*****************************************************************
      """.stripMargin
    )
    /**
     * startupTask holds an ActorRef, and execute() sends an Execute message
     * to the ActorRef in the ask pattern to let its Actor do the work
     */
    startupTask.execute().onComplete { res => println(res) }
    startupTask.execute().onComplete { res => println(res) }
    startupTask.execute().onComplete { res => println(res) }
    startupTask.execute()
  }
}

object StartupApp {
  def main(args: Array[String]): Unit = {
    val system: ActorSystem = ActorSystem("StartupApp")
    try{
      val app = new StartupApp(system)
      Await.result(app.run(), 8 seconds)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }

  }
}
