package my.grafter
import org.zalando.grafter.GenericReader._
import cats.data.Reader

case class ApplicationConfig(http: HttpConfig, db: DbConfig)

object ApplicationConfig {

//  implicit def readerHttpConfig: Reader[ApplicationConfig, HttpConfig] =
//    genericReader //org.zalando.grafter.GenericReader._
  /**
   *  implicit def genericReader[R, A, Repr](implicit
   *    gen:  Generic.Aux[A, Repr],
   *    repr: Lazy[Reader[R, Repr]]
   *  ): Reader[R, A]
   */

//  implicit def readerDbConfig: Reader[ApplicationConfig, DbConfig] =
//    genericReader //org.zalando.grafter.GenericReader._

  val prod: ApplicationConfig = ApplicationConfig(
    http = HttpConfig("localhost", 8080),
    db   = DbConfig("jdbc:localhost/database")
  )
}
