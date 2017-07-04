package my.functional.monoids

import my.wrapper.Wrapper

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
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

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  // (f compose g)(x) = f(g(x))
  def endoMonoidLeftComposeRight[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A=>A, g: A=>A) = f compose g
    val zero = (a: A) => a
  }

  // (f andThen g)(x) = g(f(x))
  def endoMonoidLeftAndThenRight[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A=>A, g: A=>A) = f andThen  g
    val zero = (a: A) => a
  }

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
   *
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
  /**
   *
   */


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
}

object MonoidMain {

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
  }
}
