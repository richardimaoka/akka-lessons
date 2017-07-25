package my.cats.herding.day1

object EqApp {
  import cats._, cats.data._, cats.implicits._
  import cats._
  import cats.data._
  import cats.implicits._

  def main(args: Array[String]): Unit ={
    /**
     * Cats provides === instead of Scala std's ==
     *
     * final class EqOps[A: Eq](lhs: A) {
     *   def ===(rhs: A): Boolean = macro Ops.binop[A, Boolean]
     *   def =!=(rhs: A): Boolean = macro Ops.binop[A, Boolean]
     * }
     */
    1 === 1

    /**
     * [error] EqApp.scala:18: type mismatch;
     * [error]  found   : String("foo")
     * [error]  required: Int
     * [error]     1 === "foo"
     * [error]           ^
     * [error] one error found
     */
    //1 === "foo"

    /**
     * == from std Scala only gives a warning, not an error
     *
     * [warn] EqApp.scala:28: comparing values of types Int and String using `==' will always yield false
     * [warn]     1 == "foo"
     * [warn]       ^
     * [warn] one warning found
     */
    //1 == "foo"

    /**
     * Cats provides =!= instead of Scala std's !=
     */
    (Some(1): Option[Int]) =!= (Some(2): Option[Int])

    /**
     * Compiler error when using Cats =!= for non-compatible types
     */
    //(Some(1): Option[Int]) =!= (Some(2): Option[Double])


    /**
     * Woah this doesn't even give a warning!
     */
    (Some(1): Option[Int]) != (Some(2): Option[Double])
  }
}
