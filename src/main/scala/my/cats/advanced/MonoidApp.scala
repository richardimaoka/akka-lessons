package my.cats.advanced

import my.wrapper.Wrapper

object MonoidApp {

  def definition(): Unit ={
    /**
     * Formally, a monoid for a type A is:
     *   1. an operation combine with type (A,A) => A
     *   2. an element empty of type A
     */

    trait Monoid[A] {
      def combine(x: A, y: A): A
      def empty: A
    }

    /**
     * In addition to providing these operations,
     * monoids must formally obey several laws.
     */
    def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
      // x combine ( y combine z ) == ( x combine y ) combine z

    def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
      (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
    }
      // x combine empty == x
      // empty combine x == x

    implicit val intSubtractionMonoid = new Monoid[Int] {
      def combine(x: Int, y: Int): Int = x - y
      def empty: Int = 0
    }

    /**
     * Does not hold.
     *   (1 - 2) - 3
     * is different from :
     *    1 - (2 - 3)
     */
    println(associativeLaw(1,2,3))

    println(identityLaw(3))
  }

  def main(args: Array[String]): Unit ={
    // "One" ++ "two" // No ++ for String :|
    Wrapper("definition")(definition)
  }

}
