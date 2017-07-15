package my.cats.examples

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/identity
 * The identity monad can be seen as the ambient monad that encodes the effect of having no effect.
 * WHAT !?!?! ??? ?? ??? ??
 *
 * It is ambient in the sense that plain pure values are values of Id.
 */
object IdentityApp {
  import cats._

  def testId(): Unit ={
    /**
     * That is to say that the type Id[A] is just a synonym for A
     */
    type Id[A] = A

    val x: Id[Int] = 1
    val y: Int = x

    // 1, int
    println(s"${x}, ${x.getClass}") //because type Id[A] = A, type Id[Int] = Int

    // 1, int
    println(s"${y}, ${y.getClass}")
  }

  def testFunctor(): Unit ={
    /**
     * Using this type declaration, we can treat our Id type constructor as a Monad and as a Comonad
     */
    type Id[A] = A
    import cats.Functor

    val one: Int = 1
    println(Functor[Id].map(one)(_ + 1))

    /**
     * The below will cause a compile error:
     *   [error] Int takes no type parameters, expected: one
     *   [error]     println(Functor[Int].map(one)(_ + 1))
     */
    // println(Functor[Int].map(one)(_ + 1))
  }

  def testApplicative(): Unit ={
    type Id[A] = A
    println(Applicative[Id].pure(42))

    /**
     *  [error] Int takes no type parameters, expected: one
     *  [error]   println(Applicative[Int].pure(42))
     */
    // println(Applicative[Int].pure(42))
  }

  def testMonad(): Unit = {
    import cats.Monad

    type Id[A] = A
    val one: Int = 1

    // 2
    println(Monad[Id].map(one)(_ + 1))

    // 2
    println(Monad[Id].flatMap(one)(_ + 1))

    // compile error like above test cases
    // println(Monad[Int].map(one)(_ + 1))
  }

  def main(args: Array[String]): Unit = {
    Wrap("testId")(testId)
    Wrap("testFunctor")(testFunctor)
    Wrap("testApplicative")(testApplicative)
    Wrap("testMonad")(testMonad)
  }
}
