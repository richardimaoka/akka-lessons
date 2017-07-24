package my.grafter

case class ApplicationConfig(http: HttpConfig, db: DbConfig)

object ApplicationConfig {

  val prod: ApplicationConfig = ApplicationConfig(
    http = HttpConfig("localhost", 8080),
    db   = DbConfig("jdbc:localhost/database")
  )
}
