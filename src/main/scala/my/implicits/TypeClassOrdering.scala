package my.implicits

/**
 * http://kmizu.hatenablog.com/entry/2017/05/23/160923
 */

object TypeClassOrdering {
  def main(args: Array[String]): Unit ={

    case class MTuple[A, B](_1: A, _2: B)

    /**
     * The below is just not possible, since you don't know what members A and B have
     * so no comparison logic can be implemeted
     */
//    implicit def tupleOrdering[A, B]: Ordering[MTuple[A, B]] =
//      new Ordering[MTuple(A, B)] {
//        override def compare(x: MTuple[A, B], y: MTuple[A, B]): Int = {
//        ???
//        }
//      }

    /**
     * The magic of available Ordering[Int], which is discussed later in this code,
     * is made here.
     *
     * "Where does Scala look for implicits?"
     * http://docs.scala-lang.org/tutorials/FAQ/finding-implicits
     */
    import scala.math.Ordering

    /**
     * This is an **instance** of a type class
     *
     * A type class instance is implicitly instantiated (declared) (i.e.) implicit def tupleOrdering
     * So that it can be passed as an implicit parameter of a different function.
     *
     * This case is bit special as the below type class *instance* also expects implicit parameters.
     * (i.e.) type class instance, dependent on other type classes.
     */
    implicit def tupleOrdering[A, B](implicit a: Ordering[A], b: Ordering[B]): Ordering[MTuple[A, B]] =
      new Ordering[MTuple[A, B]] {
        override def compare(x: MTuple[A, B], y: MTuple[A, B]): Int = {
          val MTuple(x1, x2) = x
          val MTuple(y1, y2) = y
          val r1 = a.compare(x1, y1)
          if(r1 < 0) {
            -1
          } else if(r1 > 0){
            1
          } else {
            b.compare(x2, y2)
          }
        }
      }

    /**
     * You see the power of this ...??
     * You DID NOT (re-)implement Ordering[Int] by yourself
     * but that is automatically resolved !!
     */
    val a = List(MTuple(1, 2), MTuple(4, 3)).sorted  // => List(MTuple(1, 2), MTuple(4, 3))
    println(a)
    val b = List(MTuple(4, 3), MTuple(1, 2)).sorted  // => List(MTuple(1, 2), MTuple(4, 3))
    println(b)

    // => error: No implicit Ordering defined for MTuple[Object,Object].
    // List(MTuple(new Object, new Object), MTuple(new Object, new Object)).sorted
  }
}
