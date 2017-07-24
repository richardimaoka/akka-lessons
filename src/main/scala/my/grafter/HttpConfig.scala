package my.grafter

import cats.data.Reader

case class HttpConfig(host: String, port: Int)

object HttpConfig {
  // the HttpConfig is extracted directly from the application config
  def reader: Reader[ApplicationConfig, HttpConfig] =
    Reader(_.http)
}
