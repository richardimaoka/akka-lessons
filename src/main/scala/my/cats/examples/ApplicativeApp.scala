package my.cats.examples

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/applicative
 * Applicative extends Apply by adding a single method, pure:
 *
 *   def pure[A](x: A): F[A]
 */
object ApplicativeApp {
  import cats._
  import cats.implicits._

  /**
   * This pure() method takes any value and returns the value in the context of the functor.
   * For many familiar functors, how to do this is obvious.
   *
   */
  def pureTest(): Unit ={
    /**
     * For Option, the pure operation wraps the value in Some:
     */
    // Some(1)
    println(Applicative[Option].pure(1))

    /**
     * For List, the pure operation returns a single element List:
     */
    // List(1)
    println(Applicative[List].pure(1))
  }

  /**
   * Like Functor and Apply, Applicative functors also compose naturally with each other
   */
  def composeTest(): Unit ={
    // List(Some(1))
    println((Applicative[List] compose Applicative[Option]).pure(1))
  }

  def monadTest(): Unit ={
    /**
     * Applicative is a generalization of Monad, so both Applicative and Monad
     * has the same pure() method
     */
    // Some(1)
    println(Monad[Option].pure(1))

    // Some(1), the same result as Monad
    println(Applicative[Option].pure(1))
  }

  def main(args: Array[String]): Unit = {
    Wrap("pureTest")(pureTest)
    Wrap("composeTest")(composeTest)
    Wrap("monadTest")(monadTest)
  }
}
