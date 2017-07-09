package my.functional.monoids

import my.wrapper.Wrapper

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  private var flagPrintRecurse: Boolean = false
  private var flagPrintConstructor: Boolean = false

  def printRecurse(f: => Unit): Unit = {
    flagPrintRecurse = true
    f
    flagPrintRecurse = false
  }

  def printConstractor(f: => Unit): Unit = {
    flagPrintConstructor = true
    f
    flagPrintConstructor = false
  }

  /***************************************************************************
   *
   * List of Monoid instances for String, Int, List, Option, ..
   *
   ***************************************************************************/
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  implicit def intMonoid[A] = new Monoid[Int] {
    def op(i: Int, j: Int) = i + j
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int]{
    def op(i: Int, j: Int) = i * j
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
    def op(x: Boolean, y: Boolean) = x || y
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]{
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }

  /***************************************************************************
   *
   * Non-commutative Monid example, Option
   *
   ***************************************************************************/
  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  def optionMonoidLeftOrElseRight[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  def optionMonoidRightOrElseLeft[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = y orElse x
    val zero = None
  }

  /***************************************************************************
   *
   * Special monoid, endoMonid which is also a non-commutative example,
   * and a Monoid on a function (A => A)
   *
   ***************************************************************************/
  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g
    val zero = (a: A) => a
  }

  // (f compose g)(x) = f(g(x))
  def endoMonoidLeftComposeRight[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A=>A, g: A=>A): A => A = f compose g
    val zero = (a: A) => a
  }

  // (f andThen g)(x) = g(f(x))
  def endoMonoidLeftAndThenRight[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A=>A, g: A=>A): A => A = f andThen  g
    val zero = (a: A) => a
  }

  /***************************************************************************
   *
   * Below is quizes for foldMap, foldLeft and foldRight
   *
   ***************************************************************************/
  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  /**
   * foldMap here is foldLeft, whose (z: B) parameter is "the zero element" of a Monoid.
   *
   * As Monoid is defined by:
   *   1. op
   *   2. type
   *
   * that means once you can specify (as obviously you can see in the signature)
   *   A. a Monoid,
   *   B. and give a List[A] whose A is the type in 2. of Monoid,
   *   C. f: A => B
   *
   * then foldMap can be defined.
   *
   * A doesn't have (belong to??) any Monoid, but there is f: A => B where B has a Monoid.
   * foldMap is foldLeft:
   *    X: whose initial value is the zero element of B
   *    Y: the list is already given as List[A]
   *    Z: and each element: A of List[A] is converted by f, and succeedingly foldLeft'ed
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  /**
   * In terms of its signature, (f: (B, A) => B) is different from foldLeft (order of A and B)
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  /**
   * In terms of its signature, (f: (B, A) => B) is different from foldRight (order of A and B)
   */
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0){
      if(flagPrintRecurse)
        println(s"foldMapV called for zero length seq, resulting in ${m.zero}")
      m.zero
    }
    else if (as.length == 1){
      val onlyElem = as(0)
      if(flagPrintRecurse)
        println(s"foldMapV called for only elem = ${onlyElem}")
      f(onlyElem)
    }
    else {
      val (l, r) = as.splitAt(as.length / 2)
      if(flagPrintRecurse)
        println(s"foldMapV called for l=${l} r=${r}")
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      //Stub("ard-i") + Stub("maok") = Stub("ard-imaok")
      case (Stub(c), Stub(d)) => Stub(c + d)
      //Stub("ard-i") + Part("maoka", 2, "gr") = Part("ard-imaok", 2, "gr")
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      //Part("thor", 2, "richard-i") + Stub("maok") = Part("thor", 2, "richard-imaok")
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      //Part("thor", 2, "richard-i") + Part("maoka", 3, "gr") = Part("thor", 6, "gr")
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object NonWorkingFoldable extends Foldable[List]

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `op`. That kind of object (a monoid without a `zero`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

object MonoidMain {
  import Monoid._

  def decomposeFoldRight() {
    //foldLeft[Int, Int](as: List[Int])(z: Int)( f(Int, Int) => Int)
    val a = Monoid.foldRight(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y) = ${a}")

    val as1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val z1  = 20
    val f1  = (x: Int, y: Int) => x + y
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y) = ${Monoid.foldRight(as1)(z1)(f1)}")

    val endo = Monoid.endoMonoid[Int]
    println(s"Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.endoMonoid[Int])(20)(((x,y)=>x+y).curried(20))")
    println(s"Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.endoMonoid[Int])(20)( x=>x+20 ) = ${Monoid.foldMap(as1, endo)(f1.curried)(20)}")

    val curry = f1.curried(20)
    println( s"curry = ${curry(10)}")

    val spicycurry = (f1.curried)(20)
    println( s"spicycurry = ${spicycurry(40)}")

    //foldLeft[Int, String](as: List[Int])(z: String)( f(Int, String) => Int)
    val b = Monoid.foldRight(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))("Equation: ")( (x,y) => x + " + " + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(''Equation: '')( (x,y) => x + y) = ${b}")

  }

  def testFoldMap(): Unit ={
    val a: Int = Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.intMonoid)(x => x*2)
    println(s"Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.intMonoid)(x => x*2) = ${a}")

    val b: String = Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.stringMonoid)(x => x.toString)
    println(s"Monoid.foldMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Monoid.stringMonoid)(x => x.toString) = ${b}")

    /**
     * The below is a true case where type A (= MyType) doesn't have a Monoid,
     * which will be converted by f to type B (= String)
     * @param i
     */
    case class MyType(i: Int)
    val c: String = Monoid.foldMap(
      List(
        MyType(1),
        MyType(2),
        MyType(3),
        MyType(4),
        MyType(5),
        MyType(6),
        MyType(7),
        MyType(8),
        MyType(9),
        MyType(10)
      ),
      Monoid.stringMonoid
    )( x => (x.i*x.i).toString + ", " ) //f
  }

  def testFoldLeft(): Unit ={
    //foldLeft[Int, Int](as: List[Int])(z: Int)( f(Int, Int) => Int)
    val a = Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y) = ${a}")

    //foldLeft[Int, String](as: List[Int])(z: String)( f(String, Int) => Int)
    val b = Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))("Equation: ")( (x,y) => x + " + " + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(''Equation: '')( (x,y) => x + y) = ${b}")
  }

  def testFoldRight(): Unit ={
    //foldLeft[Int, Int](as: List[Int])(z: Int)( f(Int, Int) => Int)
    val a = Monoid.foldRight(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(20)( (x,y) => x + y) = ${a}")

    //foldLeft[Int, String](as: List[Int])(z: String)( f(Int, String) => Int)
    val b = Monoid.foldRight(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))("Equation: ")( (x,y) => x + " + " + y)
    println(s"Monoid.foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(''Equation: '')( (x,y) => x + y) = ${b}")
  }

  def testDual(): Unit = {
    val a = Monoid.dual(Monoid.intMonoid).op(1, 2);
    println("Monoid.dual(Monoid.intMonoid).op(1, 2) = " + a)

    val m = Monoid.dual(Monoid.stringMonoid)
    val x = m.op("aaa", "bbb")
    println("Dual Monoid created by Monoid.stringMonid has op(aaa, bbb) = " + x)
  }

  def testEndoMonoid(): Unit ={
    val a = Monoid.endoMonoid[Int]
    val b = a.op(x => x*x, y => y+2)

    println("Monoid.endoMonoid[Int].op(x => x*x, y => y+2)(8) = (8+2)*(8+2) = " + b(8) + " // f compose g = f(g())")

    val c = Monoid.endoMonoidLeftAndThenRight[Int]
    val d = c.op(x => x*x, y => y+2)

    println("Monoid.endoMonoid[Int].op(x => x*x, y => y+2)(8) = (8*8)+2     = " + d(8) + "  // f andThen g = g(f())")
  }

  def testStringFoldMapV(): Unit = {
    case class MyStringHolder(str: String)
    val list: Array[MyStringHolder] = Array(
      MyStringHolder("a"),
      MyStringHolder("b"),
      MyStringHolder("c"),
      MyStringHolder("d"),
      MyStringHolder("e"),
      MyStringHolder("f"),
      MyStringHolder("g")
    )

    printRecurse{
      println(foldMapV(list, stringMonoid)(holder => holder.str))
    }
  }

  def testCount(): Unit = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    println( foldMapV("aaa bbb ccc".toIndexedSeq, wcMonoid)(wc) )
    println( foldMapV("aaa bbb ccc ddd".toIndexedSeq, wcMonoid)(wc) )
    println( foldMapV("aaa bbb ccc d dd ".toIndexedSeq, wcMonoid)(wc) )
    println( foldMapV("aaa bbb ccc  sd   d".toIndexedSeq, wcMonoid)(wc) )
    println( foldMapV("aaa bbb ccc richard-imaoka".toIndexedSeq, wcMonoid)(wc) )
  }


  def testNonWorkingFoldable(): Unit = {
    val list = List(1,2,3,4)
    /**
     * This will cause java.lang.StackOverflowError
     */
    println(NonWorkingFoldable.foldLeft(list)(0)(_ + _))
  }

  def main(args: Array[String]): Unit = {
    println( s"Some(1) orElse Some(2) = ${Some(1) orElse Some(2)}" )
    println( s"None orElse Some(2)    = ${None orElse Some(2)}" )
    println( s"Some(1) orElse None    = ${Some(1) orElse None}" )
    println( s"None orElse None       = ${None orElse None}" )

    println(s"endoMonoid1: ${Monoid.endoMonoid[Int].op(x => x*2, y => y*3)(10)}")
    println(s"endoMonoid2: ${Monoid.endoMonoidLeftComposeRight[Int].op(x => x*x, y => y*3)(1)}")
    println(s"endoMonoid3: ${Monoid.endoMonoidLeftAndThenRight[Int].op(x => x*x, y => y*3)(1)}")

    Wrapper("testFoldMap")(testFoldMap)
    Wrapper("testFoldLeft")(testFoldLeft)
    Wrapper("testFoldRight")(testFoldRight)

    Wrapper("testDual")(testDual)
    Wrapper("testEndoMonoid")(testEndoMonoid)

    Wrapper("decomposeFoldRight")(decomposeFoldRight)
    Wrapper("testStringFoldMapV")(testStringFoldMapV)

    Wrapper("testCount")(testCount)
    //Wrapper("testNonWorkingFoldable")(testNonWorkingFoldable)
  }
}
