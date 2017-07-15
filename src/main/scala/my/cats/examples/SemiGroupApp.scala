package my.cats.examples

import cats.kernel.Semigroup
import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/semigroup
 * A semigroup for some given type A has a single operation (which we will call combine),
 * which takes two values of type A, and returns a value of type A.
 * This operation must be guaranteed to be associative.
 *
 *   ((a combine b) combine c)
 *
 * is equivalent to
 *
 *   (a combine (b combine c))
 */
object SemiGroupApp {
  import cats.implicits._

  def testMap(): Unit = {
    val a = Map("foo" -> Map("bar" -> 5))
    val b = Map("foo" -> Map("bar" -> 6), "baz" -> Map[String,Int]())
    println( a combine b )
    /**
     * Map(foo -> Map(bar -> 11), baz -> Map())
     *                       ^^this added up (combined) 5 + 6
     */

    val c = Map("foo" -> List(1, 2))
    val d = Map("foo" -> List(3, 4), "bar" -> List(42))
    println( c combine d )
    /**
     * Map(foo -> List(1, 2, 3, 4), bar -> List(42))
     *            ^^List concat (i.e. combine) happened in side
     */

    val e = Map("foo" -> Map("bar" -> 5))
    val f = Map("foo" -> Map("bar" -> 6), "baz" -> Map())
    println( e ++ f )
    /**
     * Map(foo -> Map(bar -> 6), baz -> Map())
     *                       ^^see this is 6, NOT 5 + 6 = 11 here
     */

    val g = Map("foo" -> List(1, 2))
    val h = Map("foo" -> List(3, 4), "bar" -> List(42))
    println( g ++ h )
    /**
     * Map(foo -> List(3, 4), bar -> List(42))
     *             ^^see this is NOT concatenated (NOT combined)
     */
  }

  def testCombine(): Unit = {
    println(Semigroup[Int].combine(1, 2)) //3

    println(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) ) //List(1,2,3,4,5,6)

    println(Semigroup[Option[Int]].combine(Option(1), Option(2))) //Some(3)

    println(Semigroup[Option[Int]].combine(Option(1), None)) //Some(1)

    // 67 = (6+1) combine (6+10)
    // combine is DIFFERENT from compose
    println(
      Semigroup[Int ⇒ Int]
        .combine(
          { (x: Int) ⇒ x + 1 },
          { (x: Int) ⇒ x * 10}
        ).apply(6)
    )
  }

  def testOption: Unit = {
    val one:  Option[Int] = Option(1)
    val two:  Option[Int] = Option(2)
    val none: Option[Int] = None

    // Some(3)
    println(one |+| two) //|+| is equivalent to `combine`

    // Some(2)
    println(none |+| two)

    // None
    println(none |+| none)

    // Some(2)
    println(two |+| none)
  }

  def main(args: Array[String]): Unit ={
    Wrap("testMap")(testMap)
    Wrap("testCombine")(testCombine)
    Wrap("testOption")(testOption)
  }

}
