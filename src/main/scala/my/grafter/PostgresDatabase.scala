package my.grafter

import cats.data.Reader

case class PostgresDatabase(config: DbConfig)

object PostgresDatabase {
  def reader: Reader[ApplicationConfig, PostgresDatabase] =
    DbConfig.reader.map(PostgresDatabase.apply)
}
