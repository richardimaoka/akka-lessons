package my.grafter

import cats.data.Reader
import org.zalando.grafter.GenericReader._

case class PostgresDatabase(config: DbConfig)

object PostgresDatabase {
  implicit def reader: Reader[ApplicationConfig, PostgresDatabase] =
    genericReader

//  def reader: Reader[ApplicationConfig, PostgresDatabase] =
//    DbConfig.reader.map(PostgresDatabase.apply)
}
