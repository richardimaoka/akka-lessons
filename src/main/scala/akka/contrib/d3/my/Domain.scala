package akka.contrib.d3.my

import akka.NotUsed
import akka.actor._
import akka.contrib.d3.query.ReadJournalProvider
import akka.contrib.d3.writeside.{AggregateManagerProvider, LocalAggregateManagerProvider}
import akka.contrib.d3.{AggregateEntity, AggregateEvent, AggregateRef, AggregateSettings, EventStreamElement, EventStreamSettings, Tag, query}
import akka.persistence.query.Offset
import akka.stream.scaladsl.Source
import akka.util.Reflect
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.control.{NoStackTrace, NonFatal}

object Domain extends ExtensionId[Domain] with ExtensionIdProvider {
  override def get(system: ActorSystem): Domain = super.get(system)

  override def lookup(): ExtensionId[_ <: Extension] = Domain

  override def createExtension(system: ExtendedActorSystem): Domain = {
    val cl = findClassLoader()
    val appConfig = system.settings.config
    new Domain(system, appConfig, cl)
  }

  final class Settings(classLoader: ClassLoader, cfg: Config) {
    val config: Config = {
      val config = cfg.withFallback(ConfigFactory.defaultReference(classLoader))
      config.checkValid(ConfigFactory.defaultReference(classLoader), "akka.contrib.d3.writeside")
      config
    }

    import config._

    val topology: String =
      getString("akka.contrib.d3.topology") match {
        case "local"   ⇒ "local"
        case "cluster" ⇒ "cluster"
        case other     ⇒ throw new IllegalArgumentException(s"Unknown value $other for setting akka.contrib.d3.topology")
      }

    val amProviderClass: String =
      Try(getString("akka.contrib.d3.writeside.provider")).toOption.getOrElse(topology) match {
        case "local"   ⇒ classOf[LocalAggregateManagerProvider].getName
        case "cluster" ⇒ "akka.contrib.d3.writeside.ClusterAggregateManagerProvider"
        case fqcn      ⇒ fqcn
      }

    val readJournalProviderClass: String =
      getString("akka.contrib.d3.query.provider") match {
        case "empty"     ⇒ classOf[query.EmptyReadJournalProvider].getName
        case "in-memory" ⇒ "akka.contrib.d3.query.InMemoryReadJournalProvider"
        case "cassandra" ⇒ "akka.contrib.d3.query.CassandraReadJournalProvider"
        case fqcn        ⇒ fqcn
      }
  }

  private[d3] def findClassLoader(): ClassLoader = Reflect.findClassLoader()
}

class Domain(
  val system:        ExtendedActorSystem,
  applicationConfig: Config,
  classLoader:       ClassLoader ) extends Extension {
  import Domain._

  final val settings: Settings = new Settings(classLoader, applicationConfig)

  private val registeredTypeNames: ConcurrentMap[String, ClassTag[_]] = collection.concurrent.TrieMap()
  private val aggregateManagers: ConcurrentMap[ClassTag[_], ActorRef] = collection.concurrent.TrieMap()
  private val aggregateSettings: ConcurrentMap[ClassTag[_], AggregateSettings] = collection.concurrent.TrieMap()

  def register[E <: AggregateEntity](
    entityFactory: E#Id ⇒ E,
    name:          Option[String],
    settings:      Option[AggregateSettings]
  )( implicit ect: ClassTag[E] ): Domain = {
    println(s"Domain: register(entityFactory, name = ${name}, settings)(implicit ect=${ect}) called")

    val aggregateName = name.getOrElse(ect.runtimeClass.getName.toLowerCase)
    val aggregateSetting = settings.getOrElse(AggregateSettings(aggregateName, system.settings.config))

    val alreadyRegistered = registeredTypeNames.putIfAbsent(aggregateName, ect)
    alreadyRegistered match {
      case Some(rct) if !rct.equals(ect) ⇒
        throw new IllegalArgumentException(
          s"The AggregateName [$aggregateName] for aggregate ${ect.runtimeClass.getSimpleName} is not unique. " +
            s"It is already for ${rct.runtimeClass.getSimpleName}. Use the name argument to define a unique name."
        ) with NoStackTrace
      case _ ⇒
        println(s"Domain: alreadyRegisered = ${alreadyRegistered}")

        /**
         *  Concurrent Map !
         *    private val aggregateManagers: ConcurrentMap[ClassTag[_], ActorRef] = collection.concurrent.TrieMap()
         *    private val aggregateSettings: ConcurrentMap[ClassTag[_], AggregateSettings] = collection.concurrent.TrieMap()
         *
         *  putIfAbsent() puts an entry for the key = ect if absent
         */
        val aggregaterManagerRef = aggregateManagerProvider.aggregateManagerRef[E](entityFactory, name, aggregateSetting)
        println(s"Domain: aggregateManagerProvider = ${aggregateManagerProvider}")
        println(s"Domain: aggregaterManagerRef = ${aggregaterManagerRef}")
        aggregateManagers.putIfAbsent(ect, aggregaterManagerRef)
        aggregateSettings.putIfAbsent(ect, aggregateSetting)
    }

    this
  }

  final def register[E <: AggregateEntity]( entityFactory: E#Id ⇒ E, name: String)(implicit ect: ClassTag[E]): Domain =
    register[E](entityFactory, Some(name), None)

  def aggregateRef[E <: AggregateEntity](id: E#Id)(implicit ec:  ExecutionContext, ect: ClassTag[E]): AggregateRef[E] = {
    val aggregateManager = aggregateManagers(ect)
    val settings = aggregateSettings(ect)

    /**
     * AggregateRef has methods similar to Actor (!, tell, ?, ...) and holds aggregateManager inside
     * aggregateManager instance is only one, for a single Domain/ClassTag[E] pair
     */
    println(s"Domain: aggregateRef(${id}) gets a AggregateRef which holds aggregateManager of type ${aggregateManager.getClass} for ect = ${ect}")
    AggregateRef[E](id, aggregateManager, settings.askTimeout)
  }

  def eventStream[E <: AggregateEvent](
                                                 tag:        Tag,
                                                 fromOffset: Offset,
                                                 name:       Option[String],
                                                 settings:   Option[EventStreamSettings]
                                               ): Source[EventStreamElement[E], NotUsed] = {
    val eventStreamName = name.getOrElse(tag.value)
    val eventStreamSettings = settings.getOrElse(EventStreamSettings(eventStreamName, system.settings.config))

    readJournalProvider.readJournal(eventStreamSettings.readJournalPluginId.getOrElse(readJournalProvider.defaultReadJournalPluginId)).eventsByTag(tag.value, fromOffset)
      .map { envelope ⇒
        EventStreamElement[E](
          envelope.persistenceId,
          envelope.event.asInstanceOf[E],
          envelope.offset
        )
      }
  }

  import settings._

  private val dynamicAccess: DynamicAccess = system.dynamicAccess

  private val aggregateManagerProvider: AggregateManagerProvider = try {
    val arguments = Vector(
      classOf[ExtendedActorSystem] → system
    )

    dynamicAccess.createInstanceFor[AggregateManagerProvider](amProviderClass, arguments).get
  } catch {
    case NonFatal(e) ⇒
      throw e
  }

  private val readJournalProvider: ReadJournalProvider = try {
    val arguments = Vector(
      classOf[ExtendedActorSystem] → system
    )

    dynamicAccess.createInstanceFor[ReadJournalProvider](readJournalProviderClass, arguments).get
  } catch {
    case NonFatal(e) ⇒
      throw e
  }

}
