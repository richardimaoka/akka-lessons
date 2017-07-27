package my.cats.officialdoc

object IdApp {

  def main(args: Array[String]): Unit = {
    //Id is defined exactly as follows in cats
    //type Id[A] = A

    import cats._
    // import cats._

    val x: Id[Int] = 1
    // x: cats.Id[Int] = 1

    val y: Int = x
    // y: Int = 1

    import cats.Functor
    // import cats.Functor

    val one: Int = 1
    // one: Int = 1

    /**
     * Functor has the following signature of map
     *   def map[A, B](fa: F[A])(f: A => B): F[B]
     *
     * A = Int
     * B = Int
     * F = Id
     *
     * So,
     *
     *   def map[Int, Int](fa: Id[Int])(f: Int => Int): Id[Int]
     *   def map[Int, Int](fa: Int)(f: Int => Int): Int
     *   map(one = 1)((i: Int): Int => i + 1): Int
     */
    val r1 = Functor[Id].map(one)(_ + 1)
    println(r1)
    // res0: cats.Id[Int] = 2

    val r2 = Functor[Id].map(one)(_.toString + " yay!")
    println(r2)
    // res0: cats.Id[String] = 1 yay!

    /**
     * Signatures of map, flatMap, and conflatMap of Id
     */

    def map[A, B](fa: Id[A])(f: A => B): Id[B] = ???
    //  map[A, B](fa: A)(f: A => B): B

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = ???
    //  flatMap[A, B](fa: A)(f: A => B): B
    //  See flatMap becomes same as map

    def coflatMap[A, B](a: Id[A])(f: Id[A] => B): Id[B] = ???
    //  coflatMap[A, B](a: A)(f: A => B): B
    //  same for conflatMap, identical signature

    import cats.Monad
    // import cats.Monad

    Monad[Id].map(one)(_ + 1)
    // res1: cats.Id[Int] = 2

    Monad[Id].flatMap(one)(_ + 1)
    // res2: cats.Id[Int] = 2

    import cats.Comonad
    // import cats.Comonad

    Comonad[Id].coflatMap(one)(_ + 1)
    // res3: cats.Id[Int] = 2
  }
}
