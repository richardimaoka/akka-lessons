package akka.contrib.d3.my.app

import java.time.OffsetDateTime

import akka.contrib.d3.AggregateState.{Initialized, Uninitialized}
import akka.contrib.d3.{AggregateCommand, AggregateEntity, AggregateEvent, AggregateId, AggregateLike, AggregateState, Tag}

import scala.concurrent.{ExecutionContext, Future}

sealed trait InvoiceCommand extends AggregateCommand
object InvoiceCommand {

  case class Create(
                     amount: Invoice.Amount
                   ) extends InvoiceCommand

  case class Close(
                    reason: String
                  ) extends InvoiceCommand

}

sealed trait InvoiceEvent extends AggregateEvent {
  def metadata: InvoiceEvent.Metadata
}
object InvoiceEvent {

  case class Metadata(
                       aggregateId: Invoice.Id,
                       timestamp:   OffsetDateTime,
                       tags:        Set[Tag]
                     )

  case class Created(
                      amount:   Invoice.Amount,
                      metadata: InvoiceEvent.Metadata
                    ) extends InvoiceEvent

  case class Closed(
                     reason:   String,
                     metadata: InvoiceEvent.Metadata
                   ) extends InvoiceEvent

}


case class Invoice(
                    id:       Invoice.Id,
                    amount:   Invoice.Amount,
                    status:   Invoice.Status,
                    metadata: Invoice.Metadata
                  ) extends AggregateLike {
  type Id = Invoice.Id
}



object Invoice {
  case class Id(value: String) extends AnyVal with AggregateId
  case class Amount(value: BigDecimal) extends AnyVal

  case class OtherId(value: String) extends AggregateId

  case class Metadata(
                       versionNr: Int,
                       updatedAt: OffsetDateTime,
                       createdAt: OffsetDateTime
                     )

  sealed trait Status
  case object Status {
    case object Open extends Status
    case object Closed extends Status
  }
}

object InvoiceEntity {
  def apply(id: Invoice.Id): InvoiceEntity =
    new InvoiceEntity(id)
}

class InvoiceEntity(id: Invoice.Id) extends AggregateEntity {
  import AggregateState._

  override type Aggregate = Invoice
  override type Command = InvoiceCommand
  override type Event = InvoiceEvent

  override def identifier: Id = id

  override def initialState: State = AggregateState.Uninitialized[Invoice](identifier)

  override def onCommand(state: AggregateState[Invoice], cmd: InvoiceCommand)(implicit ec: ExecutionContext): Future[Either[Throwable, collection.immutable.Seq[Event]]] = {
    import InvoiceCommand._
    import InvoiceEvent._
    (state, cmd) match {
      case (s: Uninitialized[Invoice], c: Create) ⇒
        Future.successful(Right(List(Created(c.amount, InvoiceEvent.Metadata(state.aggregateId, OffsetDateTime.now, Set.empty)))))
      case (s: Uninitialized[Invoice], c: Close) ⇒
        Future.successful(Left(new Exception("illegal")))
      case (s: Initialized[Invoice], c: Create) ⇒
        Future.successful(Left(new Exception("illegal")))
      case (s: Initialized[Invoice], c: Close) ⇒
        Future.successful(Right(List(Closed(c.reason, InvoiceEvent.Metadata(state.aggregateId, OffsetDateTime.now, Set.empty)))))
    }
  }

  override def onEvent(state: AggregateState[Invoice], evt: InvoiceEvent): Either[Throwable, AggregateState[Invoice]] = {
    import InvoiceEvent._
    (state, evt) match {
      case (s: Uninitialized[Invoice], e: Created) ⇒
        Right(Initialized(Invoice(state.aggregateId, e.amount, Invoice.Status.Open, Invoice.Metadata(1, e.metadata.timestamp, e.metadata.timestamp))))
      case (s: Uninitialized[Invoice], e: Closed) ⇒
        Left(new Exception("illegal"))
      case (s: Initialized[Invoice], e: Created) ⇒
        Left(new Exception("illegal"))
      case (s @ Initialized(invoice), e: Closed) ⇒
        Right(s.copy(invoice.copy(status = Invoice.Status.Closed, metadata = invoice.metadata.copy(versionNr = invoice.metadata.versionNr + 1, updatedAt = e.metadata.timestamp))))
    }
  }
}
