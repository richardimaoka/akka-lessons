package my.cats.advanced

import my.wrapper.Wrap

object MonadTransformerApp {

  def pre(): Unit ={
    case class User(name: String)

    def lookupUser(id: Long): Option[User] = Some(User("myname"))

    val a = for {
      optUser <- lookupUser(100)
    } yield optUser

    val b = lookupUser(100).map(identity)

    println(a)
    println(b)

    //    def lookupUser2(id: Long): Either[Error, Option[String]] = ???
    //    def lookupUserName(id: Long): Either[Error, Option[String]] =
    //      for {
    //        optUser <- lookupUser2(id)
    //      } yield {
    //        for {
    //          user <- optUser
    //        } yield user.name
    //    }
  }

  def transformativeExample(): Unit ={
    import cats.data.OptionT

    type ListOption[A] = OptionT[List, A]

    //We need an Applicative[ListOption] to call pure.
    import cats.syntax.applicative._
    //the implicits for OptionT also require an Applicative for List. Hence the addi onal import from cats.instances.list.
    import cats.instances.list._

    /**
     * Note how we build it from the inside out: we pass List,
     * the type of the outer monad, as a parameter to OptionT,
     * the transformer for the inner monad.
     */
    val result: ListOption[Int] = 42.pure[ListOption]
    println(result)
    // result: ListOption[Int] = OptionT(List(Some(42)))

    val a = 10.pure[ListOption]
    println(a)
    // a: ListOption[Int] = OptionT(List(Some(10)))
    val b = 32.pure[ListOption]
    println(b)
    // b: ListOption[Int] = OptionT(List(Some(32)))

    val c = a flatMap { (x: Int) =>
      b map { (y: Int) =>
        x+y }
    }
    println(c)
    //OptionT(List(Some(42)))

    val d = for{
      x <- a
      y <- b
    } yield (x+y)
    println(d)
    //OptionT(List(Some(42)))
  }

  def buildingStacks(): Unit = {
    import cats.instances.either._
    import cats.data.OptionT
    import cats.syntax.applicative._

    type Error = String
    // Create a type alias, ErrorOr, to convert Either to
    // a type constructor with a single parameter:
    type ErrorOr[A] = Either[Error, A]

    // Use ErrorOr as a type parameter to OptionT:
    type ErrorOptionOr[A] = OptionT[ErrorOr, A]

    val result1 = 41.pure[ErrorOptionOr]
    // result1: ErrorOptionOr[Int] = OptionT(Right(Some(41)))
    val result2 = result1.flatMap(x => (x + 1).pure[ErrorOptionOr])
    // result2: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))
  }

  def main(args: Array[String]): Unit = {
    Wrap("pre")(pre)
    Wrap("transformativeExample")(transformativeExample)
    Wrap("buildingStacks")(buildingStacks)
  }
}
