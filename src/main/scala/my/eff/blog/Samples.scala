package my.eff.blog

import my.wrapper.Wrap
import org.atnos.eff.MemberIn

object Samples {
  object Error {
    def apply(errorMessage: String) = new Error(errorMessage)
  }

  case class User(name: String, id: UserId, propertyId: PropertyId)
  class UserId(id: Int) {
    def get(): Int = id
  }

  case class Property(s: String, id: PropertyId)
  case class PropertyId(id: Int){
    def get(): Int = id
  }

  case class PropertyApiUrl(url: String) {
    def get: String = url
  }

  def sample1(): Unit ={
    /**
     * In a real world scenario, getUser and getProperty may be querying a database instead.
     */
    def getUser(id: UserId): Either[Error, User] =
      if (id.get > 1000)
        Right(User("Bob", id, PropertyId(1230)))
      else
        Left(Error(s"Id ${id.get} in invalid range"))

    def getProperty(id: PropertyId): Either[Error, Property] =
      if (id.get > 1000)
        Right(Property("Big house!", id))
      else
        Left(Error("Wrong URL!"))

    println(getUser(new UserId(300)))
    //Left(java.lang.Error: Id 300 in invalid range)

    println(getUser(new UserId(1300)))
    //Right(User(Bob,my.eff.blog.Samples$UserId$1@5b6b665a,PropertyId(123)))


    def getPropertyForUserId(id: UserId): Either[Error, Property] =
      for {
        user <- getUser(id)
        property <- getProperty(user.propertyId)
      } yield property

    println(getPropertyForUserId(new UserId(1300)))
    //Right(Property(Big house!,PropertyId(1230)))

    /**
     * What if we add another monad?
     */
    import cats.data.Reader
    def getProperty2(id: PropertyId): Reader[PropertyApiUrl, Either[Error, Property]] =
      Reader {
        propertyApiUrl =>
          if (propertyApiUrl.get == "https://production.property-api.com")
            Right(Property("Big house!", id))
          else
            Left(Error("Wrong URL!"))
      }

    /**
     * Same signature as before
     */
//    def getPropertyForUserId2(id: UserId): Either[Error, Property] = {
//      val errorOrUser: Either[Error, User] = getUser(id)
//      errorOrUser map { user =>
//        val readProperty: Reader[PropertyApiUrl, Either[Error, Property]] =
//          getProperty2(user.propertyId)
//        readProperty.run(PropertyApiUrl("https://production.property-api.com"))
//      }
//    }
  }

  def sample2(): Unit ={
    import org.atnos.eff.{Eff, Fx}
    import org.atnos.eff.all._
    import org.atnos.eff.syntax.all._
    import cats.data.Reader

    type AppStack = Fx.fx2[Either[Error, ?], Reader[PropertyApiUrl, ?]]

    type _either[R] = MemberIn[Either[Error, ?], R]

    def getUser[R: _either](id: UserId): Eff[R, User] =
      if (id.get > 1000)
        right(User("Bob", id, PropertyId(123)))
      else
        left(Error(s"Id ${id.get} in invalid range"))

    def getProperty(id: PropertyId): Either[Error, Property] =
      if (id.get > 1000)
        Right(Property("Big house!", id))
      else
        Left(Error("Wrong URL!"))

//    def getPropertyForUserId(id: UserId): Either[Error, Property] = {
//
//      val program: Eff[AppStack, Property] = for {
//        user <- getUser[AppStack](id)
//        property <- getProperty[AppStack](user.propertyId)
//      } yield property
//
//      val result: Either[Error, Property] = program
//        .runReader(PropertyApiUrl("https://production.property-api.com"))
//        .runEither[Error]
//        .run
//        // Execute the entire program using `run`.
//        // This can only be done after having interpreted all the effects in the stack,
//        // otherwise get a compilation error.
//
//      result match {
//        case Left(e) => println(e.msg) // log errors
//        case Right(p) => println(s"User ${id.get} owns Property ${p.id.get}")
//      }
//
//      result
//    }
  }

  def main(args: Array[String]): Unit = {
    Wrap("sample1")(sample1)
  }
}
