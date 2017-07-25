package my.cats.herding.day1

object PartialOrder {

  import cats._
  import cats.data._
  import cats.implicits._

  def main(args: Array[String]): Unit ={
    /**
     * In addition to Order, Cats also defines PartialOrder.
     *
     * def tryCompare(rhs: A): Option[Int] = macro Ops.binop[A, Option[Int]]
     */
    val a = 1 > 2.0

    /**
     * http://eed3si9n.com/herding-cats/PartialOrder.html
     *
     * PartialOrder enables tryCompare syntax which returns Option[Int].
     * According to algebra, it’ll return None if operands are not comparable.
     * It’s returning Some(-1) when comparing 1.0 and Double.NaN,
     * so I’m not sure when things are incomparable.
     */
    println(1 tryCompare 2)            //Some(-1)
    println(1.0 tryCompare Double.NaN) //Some(-1)

    println("" tryCompare " ")   //Some(-1)
    println("" tryCompare "abc") //Some(-1)
    println("bbb" tryCompare "abc") //Some(1)


    def lt[A: PartialOrder](a1: A, a2: A): Boolean = a1 <= a2

    //println(lt[Int](1, 2.0))
    /**
     * [error] PartialOrder.scala:31: type mismatch;
     * [error]  found   : Double(2.0)
     * [error]  required: Int
     * [error]     println(lt[Int](1, 2.0))
     * [error]                        ^
     */

    println(lt(1, 2)) //true
  }
}
