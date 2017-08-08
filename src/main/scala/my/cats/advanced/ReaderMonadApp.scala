package my.cats.advanced

import cats.data.Reader
import my.wrapper.Wrap

object ReaderMonadApp {
  case class Cat(name: String, favoriteFood: String)
  // defined class Cat

  def basic(): Unit ={
    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)
    println(catName)
    // catName: cats.data.Reader[Cat,String] = Kleisli(<function1>)

    val result = catName.run(Cat("Garfield", "lasagne"))
    /**
     * Extracting the Cat.name field of Cat by Reader(cat => cat.name)
     */
    println(result)
    // res0: cats.Id[String] = Garfield
  }

  def kitty(): Unit ={
    val catName: Reader[Cat, String]    = Reader(cat => cat.name)
    val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")

    val result = greetKitty.run(Cat("Heathcliff", "junk food"))
    /**
     * Extracting the Cat.name field of Cat by
     *   catName.map(name => s"Hello ${name}")
     *   = Reader(cat => cat.name).map(name => s"Hello ${name}")
     */
    println(result)
    // res1: cats.Id[String] = Hello Heathcliff


    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

    val greetAndFeed: Reader[Cat, String] =
      for {
        msg1 <- greetKitty
        msg2 <- feedKitty
      } yield s"${msg1} ${msg2}"

    println(greetAndFeed(Cat("Garfield", "lasagne")))
    // res3: cats.Id[String] = Hello Garfield Have a nice bowl of lasagne

    println(greetAndFeed(Cat("Heathcliff", "junk food")))
    // res4: cats.Id[String] = Hello Heathcliff Have a nice bowl of junk food
  }

  def exercise1(): Unit ={
    case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
    )

    /**
     * create a type alias DbReader for a Reader that consumes a Db as input.
     * This will make the rest of our code shorter:
     */
    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def findUsernameL(userId: Int): Reader[Db, Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(
      username: String,
      password: String
    ): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkPasswordL(
      username: String,
      password: String
    ): Reader[Db, Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    val db = Db(
      Map(1 -> "nishyu", 2 -> "robindu"),
      Map("nishyu" -> "passw1",  "robindu" -> "passw2")
    )

    println(findUsername(1)(db))
    //Some(nishyu)
    println(findUsername(2)(db))
    //Some(robindu)
    println(findUsername(3)(db))
    //None

    println("\ncheckPassword")
    println(checkPassword("nishyu", "passw1")(db))
    //true
    println(checkPassword("robindu", "passw2")(db))
    //true
    println(checkPassword("nishyu", "passwX")(db))
    //false

    import cats.syntax.applicative._ // for `pure`
    def checkLogin(
      userId: Int,
      password: String
    ): DbReader[Boolean] =
      for {
        // findUsername(userId) returns an Option[String],
        // so to get the String, you need to unwrap (flatMap) it
        username <- findUsername(userId)
        passwordOk <- username.map { username =>
          checkPassword(username, password)
        }.getOrElse {
          false.pure[DbReader]
        }
      } yield passwordOk

    println("\ncheckLogin")
    println(checkLogin(1, "passw1")(db))
    //true
    println(checkLogin(2, "passw2")(db))
    //true
    println(checkLogin(1, "passwX")(db))
    //false

    val db2 = Db(
      Map(
        1 -> "dade",
        2 -> "kate",
        3 -> "margo"
      ),
      Map(
        "dade" -> "zerocool",
        "kate" -> "acidburn",
        "margo" -> "secret"
      )
    )

    println("\ndb2")
    // db: Db = Db(Map(1 -> dade, 2 -> kate, 3 -> margo),Map(dade ->zerocool, kate -> acidburn, margo -> secret))
    println(checkLogin(1, "zerocool").run(db2))
    // res8: cats.Id[Boolean] = true
    println(checkLogin(4, "davinci").run(db2))
    // res9: cats.Id[Boolean] = false
    //
  }

  def exercise2(): Unit = {

  }


  def main(args: Array[String]): Unit = {
    Wrap("basic")(basic)
    Wrap("kitty")(kitty)
    Wrap("exercise1")(exercise1)
  }
}
