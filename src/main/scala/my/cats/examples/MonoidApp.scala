package my.cats.examples

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/monoid
 * Monoid extends the Semigroup type class, adding an empty method to semigroup's combine.
 *
 *  (combine(x, empty) == combine(empty, x) == x)
 */
object MonoidApp {
  import cats._
  import cats.implicits._

  def stringTest(): Unit = {
    // empty string printed out
    println(Monoid[String].empty)

    // abc
    println(Monoid[String].combineAll(List("a", "b", "c")))

    // empty string printed out
    println(Monoid[String].combineAll(List()))

  }

  def mapTest(): Unit ={
    /**
     * The advantage of using these type class provided methods, rather than the specific ones for each type,
     * is that we can compose monoids to allow us to operate on more complex types, e.g.
     *
     * Map(b -> 2, a -> 4)
     */
    println(Monoid[Map[String, Int]]
      .combineAll(
        List(
          Map("a" → 1, "b" → 2),
          Map("a" → 3)
        )
      )
    )

    // Map()
    println(Monoid[Map[String, Int]].combineAll(List()))
  }

  def listTest(): Unit = {
    val l = List(1, 2, 3, 4, 5)

    // 0
    println(Monoid[Int].empty)

    /**
     * foldMap: `fold` all elements:
     *   1. initial value = Monoid's empty
     *   2. with a map (f: A => A)
     *     2.1. accumulation and elements are both in type A
     *
     * 15 <- result of below
     */
    println(l.foldMap(identity))

    //12345
    println(l.foldMap(i ⇒ i.toString))
  }

  def monoidTupleTest(): Unit = {
    val l = List(1, 2, 3, 4, 5)

    /**
     * This actually fails to compile... hm??
     * [error]  both method catsKernelStdMonoidForTuple2 in trait TupleInstances of type [A0, A1](implicit A0: cats.kernel.Monoid[A0], implicit A1: cats.kernel.Monoid[A1])cats.kernel.Monoid[(A0, A1)]
     * [error]  and method monoidTuple of type [A, B](implicit evidence$1: cats.Monoid[A], implicit evidence$2: cats.Monoid[B])cats.Monoid[(A, B)]
     * [error]  match expected type cats.Monoid[(Int, String)]
     * [error]     println(l.foldMap(i 竍・(i, i.toString)))
     */
    // implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    //   new Monoid[(A, B)] {
    //     def combine(x: (A, B), y: (A, B)): (A, B) = {
    //       val (xa, xb) = x
    //       val (ya, yb) = y
    //       (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
    //     }
    //   def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
    // }
    
    //(15,12345)
    println(l.foldMap(i ⇒ (i, i.toString)))
  }


  def main(args: Array[String]): Unit = {
    Wrap("stringTest")(stringTest)
    Wrap("mapTest")(mapTest)
    Wrap("listTest")(listTest)
    Wrap("monoidTupleTest")(monoidTupleTest)
  }
}
