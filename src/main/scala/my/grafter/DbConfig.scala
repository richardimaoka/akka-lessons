package my.grafter

import cats.data.Reader

case class DbConfig(url: String)

object DbConfig {
  // the DbConfig is extracted directly from the application config
  def reader: Reader[ApplicationConfig, DbConfig] =
    Reader(_.db)
}
