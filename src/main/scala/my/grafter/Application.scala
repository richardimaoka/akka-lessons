package my.grafter

import cats.data.Reader

case class Application(httpServer: HttpServer, db: PostgresDatabase)

object Application {

  // Reader has a Monad instance so we can use it in a for comprehension
  implicit def reader: Reader[ApplicationConfig, Application] =
    for {
      server   <- HttpServer.reader
      database <- PostgresDatabase.reader
    } yield Application(server, database)

  val application: Application =
    Application.reader.apply(ApplicationConfig.prod)

}
