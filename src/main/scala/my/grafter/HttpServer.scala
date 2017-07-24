package my.grafter

import cats.data.Reader
import org.zalando.grafter.GenericReader._

case class HttpServer(config: HttpConfig)

object HttpServer {

  implicit def reader: Reader[ApplicationConfig, HttpServer] =
    genericReader

//  // we can "map" on a Reader!
//  def reader: Reader[ApplicationConfig, HttpServer] =
//    HttpConfig.reader.map(HttpServer.apply)

}
