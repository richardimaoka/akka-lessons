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
    println(41.pure[ErrorOr])
    //Right(41)

    // Use ErrorOr as a type parameter to OptionT:
    type ErrorOptionOr[A] = OptionT[ErrorOr, A]

    val result1 = 41.pure[ErrorOptionOr]
    println(result1)
    // result1: ErrorOptionOr[Int] = OptionT(Right(Some(41)))
    println(result1.value)
    //Right(Some(41))

    //def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B]
    val result2 = result1.flatMap(x => (x + 1).pure[ErrorOptionOr])
    println(result2)
    // result2: cats.data.OptionT[ErrorOr,Int] = OptionT(Right(Some(42)))
    println(result2.value)
    //Right(Some(42))
  }

  def buildingStacks2(): Unit = {
    import scala.concurrent.Future
    import cats.data.EitherT
    import cats.data.OptionT

    //type FutureEitherOption[A] = OptionT[EitherT, A]
    /**
     * [error] MonadTransformerApp.scala:98:
     *           cats.data.EitherT takes three type parameters, expected: one
     * [error] type FutureEitherOption[A] = OptionT[EitherT, A]
     */

    type Error = String
    type FutureEither[A] = EitherT[Future, String, A]
    /**
     * Now this compiles
     */
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    import cats.syntax.applicative._

    val answer: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption] //OptionT[EitherT[Future, String, Int], Int]
        b <- 32.pure[FutureEitherOption] //OptionT[EitherT[Future, String, Int], Int]
      } yield a + b

    Thread.sleep(200)
    println(answer)
    //OptionT(EitherT(Future(Success(Right(Some(42))))))
    //
    //== OptionT(EitherT(
    //   Future(Success(Right(Some(42))))
    //                 ^Either^Option
    // ))
    //
    //if you do not wait
    //OptionT(EitherT(Future(<not completed>)))

    println(answer.value)
    //EitherT(Future(Success(Right(Some(42)))))

    println(answer.value.value)
    //Future(Success(Right(Some(42))))
  }

  def constructingAndUnpacking(): Unit ={
    import cats.data.OptionT
    import cats.instances.either._
    import cats.syntax.applicative._
    import cats.syntax.either._ // for foo.asRight
    import cats.syntax.option._ // for foo.some

    type ErrorOr[A] = Either[String, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    // Create using pure:
    val stack1 = 123.pure[ErrorOrOption]
    println(stack1)
    // stack1: ErrorOrOption[Int] = OptionT(Right(Some(123)))
    println(stack1.value)
    //Right(Some(123))

    // Create using apply:
    val stack2 = OptionT[ErrorOr, Int](
      123.some.asRight[String]
    )
    println(stack2)
    //OptionT(Right(Some(123)))
    println(stack2.value)
    //Right(Some(123))
  }

  def main(args: Array[String]): Unit = {
    Wrap("pre")(pre)
    Wrap("transformativeExample")(transformativeExample)
    Wrap("buildingStacks")(buildingStacks)
    Wrap("buildingStacks2")(buildingStacks2)
    Wrap("constructingAndUnpacking")(constructingAndUnpacking)
  }
}
