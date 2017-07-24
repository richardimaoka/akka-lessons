package my.grafter

import cats.data.Reader
import org.zalando.grafter.GenericReader._

case class HttpServer(config: HttpConfig)

object HttpServer {

//  implicit def reader: Reader[ApplicationConfig, HttpServer] =
//    genericReader

  /**
   * If you are creating a library, you will probably want to avoid
   * HttpServer statically dependent on the ApplicationConfig.
   *
   * To do it, lets parametrize the reader function with some config of type A:
   */
//  implicit def dependentReader[A](implicit httpConfigReader: Reader[A, HttpConfig]): Reader[A, HttpServer] =
//    genericReader

  // we can "map" on a Reader!
  def reader: Reader[ApplicationConfig, HttpServer] =
    HttpConfig.reader.map(HttpServer.apply)

}
