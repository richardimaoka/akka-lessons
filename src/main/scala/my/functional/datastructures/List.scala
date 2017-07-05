package my.functional.datastructures

import my.functional.datastructures.List.sum
import my.wrapper.Wrapper

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String =
    "List(" + List.foldRight(this, ")")( (a, str) => a.toString + "," + str )
}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => {
        println("append() executes internal append()")
        Cons(h, append(t, a2))
      }
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  /*
    Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
    a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
    returning a value just means this bug will be discovered later, further from the place where it was introduced.

    It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
    right hand side of a pattern. This makes it clear the value isn't relevant.
    */
  def tail[A](l: List[A]): List[A] = l match {
    //case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  /*
  If a function body consists solely of a match expression, we'll often put the match on the same line as the
  function signature, rather than introducing another level of nesting.
  */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    //case Nil => Nil
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  /*
  Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
  contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
  is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
  length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
  If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
  */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
  satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
  use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

}

object ListTest {
  def listTest(): Unit = {
    def z(list: List[Int]): Int = list match {
      case Cons(x, Cons(2, Cons(4, _))) => {
        println("Hit the case: Cons(x, Cons(2, Cons(4, _))) ")
        x
      }
      case Nil => {
        println("Hit the case: Nil")
        42
      }
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => {
        println("Hit the case: Cons(x, Cons(y, Cons(3, Cons(4, _))))")
        x + y
      }
      case Cons(h, t) => {
        println("Hit the case: Cons(h, t) ")
        h + sum(t)
      }
      case _ => {
        println("Hit the case: _ ")
        101
      }
    }

    println(z(List(1,2,3,4,5)))
    println(z(List(1)))
    println(z(List(3,3,3,4,3,3)))
    println(z(List(3,3,3,5,3,3)))
    println(z(List(3,3,6,3,4,3,3)))
    println(z(List(1,2)))
    println(z(List()))
  }

  def tailTest(): Unit ={
    println(s"List.tail(List(1,2,3)) = ${List.tail(List(1,2,3))}, in class = ${List.tail(List(1,2,3)).getClass}}")
  }

  def setHeadTest(): Unit ={
    println(s"List.setHead(List(1,2,3), 10) = ${List.setHead(List(1,2,3), 10)}")
  }

  def appendTest(): Unit ={
    List.append(List(1,2,3), List(1,2,3))
  }


  def main(args: Array[String]): Unit = {
    Wrapper("listTest")(listTest)
    Wrapper("tailTest")(tailTest)
    Wrapper("setHeadTest")(setHeadTest)
    Wrapper("appendTest")(appendTest)
  }
}
