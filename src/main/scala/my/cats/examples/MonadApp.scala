package my.cats.examples

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/monad
 * Monad extends the Applicative type class with a new function `flatten`.
 *
 * Flatten takes a value in a nested context (eg. F[F[A]] where F is the context)
 * and "joins" the contexts together so that we have a single context (ie. F[A]).
 *
 * The meaning of "join" varies by F[].
 */
object MonadApp {
  import cats._
  import cats.implicits._

  def simpleTest(): Unit = {
    println(Option(Option(1)).flatten)

    println(Option(None).flatten)

    println(List(List(1), List(2, 3)).flatten)

  }

  def optionMonadDefinitionTest: Unit ={
//    implicit def optionMonad(implicit app: Applicative[Option]) =
//      new Monad[Option] {
//        // Define flatMap using Option's flatten method
//        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
//          app.map(fa)(f).flatten
//        // Reuse this definition from Applicative.
//        override def pure[A](a: A): Option[A] = app.pure(a)
//      }
  }

  def optionMonadTest(): Unit ={
    println(Monad[Option].pure(42))
  }

  def listMonadTest(): Unit ={
//    implicit val listMonad = new Monad[List] {
//      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
//      def pure[A](a: A): List[A] = List(a)
//    }
  }

  def flatMapTest(): Unit ={
    println(Monad[List].flatMap(List(1, 2, 3))(x ⇒ List(x, x)))
  }

  def IFMTest(): Unit ={
    println(Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")))

    println(Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)))
  }

  def composeTest(): Unit ={
//    case class OptionT[F[_], A](value: F[Option[A]])
//
//    implicit def optionTMonad[F[_]](implicit F: Monad[F]) = {
//      new Monad[OptionT[F, ?]] {
//        def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
//        def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
//          OptionT {
//            F.flatMap(fa.value) {
//              case None => F.pure(None)
//              case Some(a) => f(a).value
//            }
//          }
//        def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
//          defaultTailRecM(a)(f)
//      }
//    }
//    println(optionTMonad[List].pure(42))
  }

  def optionTMonad(): Unit ={

  }

  def main(args: Array[String]): Unit = {
    Wrap("simpleTest")(simpleTest)
    Wrap("optionMonadTest")(optionMonadTest)
    Wrap("listMonadTest")(listMonadTest)
    Wrap("flatMapTest")(flatMapTest)
    Wrap("IFMTest")(IFMTest)
    Wrap("composeTest")(composeTest)
    Wrap("optionTMonad")(optionTMonad)
  }
}
