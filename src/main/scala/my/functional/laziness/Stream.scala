package my.functional.laziness

import Stream._
import my.functional.datastructures.List.{flagPrintConstructor, flagPrintFold}
import my.wrapper.Wrap
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    /**
     * Not tail recursive, because :: is a right-associative method ( a method ends in : in its name )
     * which means the below is interpreted as
     *
     *   to().toListRecursive.::(h())
     *
     * obviously, the last function call is not toListRecursive
     */
    case Cons(h,t) => {
      if(Stream.flagPrintRecurse)
        println(s"toListRecursive called for h = ${h()} and t = ${t()}")
      h() :: t().toListRecursive
    }
    case _ => {
      if(Stream.flagPrintRecurse)
        println(s"toListRecursive -> List()")
      List()
    }
  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => {
        if(Stream.flagPrintRecurse)
          println(s"toList -> go called for h = ${h()} and t = ${t()} => go(${t()}, ${h()} :: ${acc} })")
        go(t(), h() :: acc)
      }
      case _ => {
        if(Stream.flagPrintRecurse)
          println(s"toList -> go called for acc = ${acc}")
        acc
      }
    }
    if(Stream.flagPrintRecurse)
      println("List reversed")
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }


  /*
  Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
  calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
  we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
  at the stream at all.
*/
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => {
      if(Stream.flagPrintRecurse)
        println(s"take is called for Cons(${h()}, ${t()}) where n = ${n} => cons(${h()}, ${t}.take(${n}-1))")
      cons(h(), t().take(n - 1))
    }
    case Cons(h, _) if n == 1 => {
      if(Stream.flagPrintRecurse)
        println(s"take is called for Cons(${h()}, _) where n = ${1} => cons(${h()}, Empty)")
      cons(h(), empty)
    }
    case _ => {
      if(Stream.flagPrintRecurse)
        println(s"take is called for Empty)")
      empty
    }
  }

  /*
    Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by recursively calling
    drop on the invoked tail of a cons cell. Note that the implementation is also tail recursive.
  */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
  It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
  */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString = "Stream(...)"
}

object Stream {

  private var flagPrintRecurse: Boolean = false
  private var flagPrintConstructor: Boolean = false

  def printRecurseCalls(f: => Unit): Unit = {
    flagPrintRecurse = true
    f
    flagPrintRecurse = false
  }

  def printConstractor(f: => Unit): Unit = {
    flagPrintConstructor = true
    f
    flagPrintConstructor = false
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    /**
     * See the parameters are passed as () =>
     * to match with the signature of Cons()
     */
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  // The natural recursive solution
  def toListRecursive[A](stream: Stream[A]): List[A] = stream match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toList[A](stream: Stream[A]): List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(stream, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast[A](stream: Stream[A]): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(stream)
  }

}

object StreamTest {
  import Stream._
  def preRequisits: Unit = {
    def f(g : () => String): String = g()

    /**
     * This fails to compile:
     *   [error]  found   : () => String
     *   [error]  required: String
     *   [error]     def f(g : () => String): String = g
     */
    //println(f("s"))
  }
  def basics(): Unit ={
    val a = Stream(1,2,3)
    println(a)
    println(a.toListRecursive)
  }

  def toListTest(): Unit ={
    val a = Stream(1,2,3)
    printRecurseCalls{
      println(a.toListRecursive)
      println(a.toList)
      println(a.toListFast)
    }

    println()
    println(toListRecursive(a))
    println(toList(a))
    println(toListFast(a))
  }

  def takeTest(): Unit = {
    val a = Stream(1,2,3,4,5,6,7,8,9,10)
    printRecurseCalls{
      val atake1 = a.take(1)
      println(atake1.toList)
      println()

      val atake6 = a.take(6)
      println(atake6.toList)
    }
  }

  def main(args: Array[String]): Unit = {
    Wrap("basics")(basics)
    Wrap("preRequisits")(preRequisits)
    Wrap("toListTest")(toListTest())
    Wrap("takeTest")(takeTest)
  }
}