package my.cats.herding.day2

import simulacrum._

object MakeYourOwnTypeClass {

  def canTruthy(): Unit = {
//    @typeclass trait CanTruthy[A] { self =>
//      /** Return true, if `a` is truthy. */
//      def truthy(a: A): Boolean
//    }
//
//    object CanTruthy {
//      def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
//        def truthy(a: A): Boolean = f(a)
//      }
//    }
//
//    implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
//      case 0 => false
//      case _ => true
//    })

    // .ops is generated .. nah not generated :(
    //import CanTruthy.ops._

    //nahhhh not compiled :(
    //10.truthy
  }

  def canAppend(): Unit ={
//    @typeclass trait CanAppend[A] {
//      @op("|+|") def append(a1: A, a2: A): A
//    }
//
//    implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
//      def append(a1: Int, a2: Int): Int = a1 + a2
//    }

    // .ops is generated .. nah not generated :(
    //import CanAppend.ops._
    //nahhhh not compiled :(
    // 1 |+| 2 // 3
  }

  def main(args: Array[String]): Unit ={
  }
}
