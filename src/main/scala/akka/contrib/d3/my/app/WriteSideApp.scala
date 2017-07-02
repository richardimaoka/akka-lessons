package akka.contrib.d3.my.app

import akka.Done
import akka.actor.ActorSystem
import akka.contrib.d3.AggregateRef
import akka.contrib.d3.my.Domain
import com.typesafe.config.ConfigFactory

import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.concurrent.duration._

class WriteSideApp(system: ActorSystem) {
  implicit private val ec = system.dispatcher
  implicit private val timeout = akka.util.Timeout(10 seconds)

  private val domain = Domain(system).register[InvoiceEntity](
    entityFactory = (id: Invoice.Id) => InvoiceEntity(id),
    name = "invoices"
  )

  def run(): Future[Done] = {
    val aggregates: List[AggregateRef[InvoiceEntity]] =
      List.fill(5) {
        Random.nextInt(1000000)
      }.map { id ⇒ domain.aggregateRef[InvoiceEntity](Invoice.Id(s"$id")) }

    /**
     * Reduce many Futre's into a single
     */
    val result = Future.sequence {
      aggregates.map(aggregate ⇒
        for {
          e1 ← aggregate ? InvoiceCommand.Create(Invoice.Amount(BigDecimal(200)))
          s1 ← aggregate.state
          e2 ← aggregate ? InvoiceCommand.Close("paid")
          s2 ← aggregate.state
          q3 ← aggregate.isInitialized
        } yield (e1, s1, e2, s2, q3))
    }

    result.map { l =>
      l.foreach {
        case (e1, s1, e2, s2, q3) ⇒
          println(
            s"""
               |1. events:       $e1
               |   state:        $s1
               |2. events:       $e2
               |   state:        $s2
               |4. initialized?: $q3
           """.stripMargin
          )
      }
      Done
    }
  }
}

object WriteSideApp {
  val system: ActorSystem = ActorSystem("WriteSideApp", ConfigFactory.load("d3"))

  def main(args: Array[String]): Unit = {
    try{
      val app = new WriteSideApp(system)
      Await.result(app.run(), 8 seconds)
    }
    finally {
      println("terminating the system")
      system.terminate()
    }

  }
}
