package akka.contrib.d3.my

import akka.Done
import akka.actor.{DynamicAccess, _}
import akka.util.Reflect
import com.typesafe.config._

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
 * Akka extention's pattern = extends ExtensionId[StartupTasks] with ExtensionIdProvider
 * and implement get(), lookup(), as well as createExtention()
 */
object StartupTasks extends ExtensionId[StartupTasks] with ExtensionIdProvider {

  //ExtensionId's method, usually super.get(system) is fine
  override def get(system: ActorSystem): StartupTasks = super.get(system)

  //ExtensionIdProvider's method, usually returning the self (companion object) is okay
  override def lookup(): ExtensionId[_ <: Extension] = StartupTasks

  //ExtensionId's method
  override def createExtension(system: ExtendedActorSystem): StartupTasks = {
    val cl = findClassLoader()
    val appConfig = system.settings.config
    new StartupTasks(system, appConfig, cl)
  }

  final class Settings(classLoader: ClassLoader, cfg: Config) {
    val config: Config = {
      val config = cfg.withFallback(ConfigFactory.defaultReference(classLoader))
      config.checkValid(ConfigFactory.defaultReference(classLoader), "akka.contrib.d3.utils.startup-tasks")
      config
    }

    import config._

    val topology: String =
      getString("akka.contrib.d3.topology") match {
        case "local"   ⇒ "local"
        case "cluster" ⇒ "cluster"
        case other     ⇒ throw new IllegalArgumentException(s"Unknown value $other for setting akka.contrib.d3.topology")
      }

    val startupTaskProviderClass: String = {
      println("looking up StartupTasks.startupTaskProviderClass")

      val providerTry = Try(getString("akka.contrib.d3.utils.startup-tasks.provider"))
      providerTry match {
        case Success(providerName) => println("provider name = " + providerName)
        case Failure(_)            => println(s"provider name is not specified at akka.contrib.d3.utils.startup-tasks.provider, then topology = ${topology}")
      }

      val provider = providerTry.toOption.getOrElse(topology) match {
        case "local"   ⇒ classOf[LocalStartupTaskProvider].getName
        case "cluster" ⇒ "akka.contrib.d3.utils.ClusterStartupTaskProvider"
        case fqcn      ⇒ fqcn
      }

      println(s"resulting provider = ${provider}")
      provider
    }
  }

  def findClassLoader(): ClassLoader = Reflect.findClassLoader()
}

class StartupTasks(
  val system:        ExtendedActorSystem,
  applicationConfig: Config,
  classLoader:       ClassLoader ) extends Extension {

  import StartupTasks._

  final val settings: Settings = new Settings(classLoader, applicationConfig)

  protected val dynamicAccess: DynamicAccess = system.dynamicAccess

  /**
   * Returns a StartupTask instance - note this class (i.e. Extension) is StartupTask**s**
   * startupTaskProvider is selected dynamically at runtime by Settings of the companion object
   */
  def create(
    name:                String,
    task:                () ⇒ Future[Done],
    timeout:             FiniteDuration,
    minBackoff:          FiniteDuration,
    maxBackoff:          FiniteDuration,
    randomBackoffFactor: Double ): StartupTask =
    startupTaskProvider.startupTask(name, task, timeout, minBackoff, maxBackoff, randomBackoffFactor)

  def createTask(
              name:                String,
              task:                () ⇒ Future[Done],
              timeout:             FiniteDuration,
              minBackoff:          FiniteDuration,
              maxBackoff:          FiniteDuration,
              randomBackoffFactor: Double ): StartupTask =
    create(name, task, timeout, minBackoff, maxBackoff, randomBackoffFactor)

  import settings._

  private val startupTaskProvider: StartupTaskProvider = try {
    val arguments = Vector(
      classOf[ExtendedActorSystem] → system
    )

    dynamicAccess.createInstanceFor[StartupTaskProvider](startupTaskProviderClass, arguments).get
  } catch {
    case NonFatal(e) ⇒
      throw e
  }

}
