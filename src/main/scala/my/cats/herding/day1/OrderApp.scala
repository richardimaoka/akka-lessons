package my.cats.herding.day1

object OrderApp {
  import cats._
  import cats.data._
  import cats.implicits._

  def main(args: Array[String]): Unit ={
    val a = 1 > 2.0

    /**
     * Cats provides `compare`
     *
     * // oops, not actually ... this is from the Scala standard library...
     *
     * trait OrderedProxy[T] extends Any with Ordered[T] with Typed[T] {
     *   protected def ord: Ordering[T]
     *
     *   def compare(y: T) = ord.compare(self, y)
     * }
     */

    /**
     * Order enables compare syntax which returns Int:
     * negative, zero, or positive.
     */
    println(1 compare 2) //-1
    println(2 compare 1) //+1

    /**
     * [error] /Users/yunishiyama/akka-lessons/src/main/scala/my/cats/herding/day1/OrderApp.scala:18: type mismatch;
     * [error]  found   : Double(2.0)
     * [error]  required: Int
     * [error]     println(1 compare 2.0)
     * [error]
     */
    //println(1 compare 2.0)
  }
}
