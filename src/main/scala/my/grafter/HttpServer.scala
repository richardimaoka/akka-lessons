package my.grafter

import cats.data.Reader
import cats.implicits._

case class HttpServer(config: HttpConfig)

object HttpServer {
  // we can "map" on a Reader!
  def reader: Reader[ApplicationConfig, HttpServer] =
    HttpConfig.reader.map(HttpServer.apply)

}
