package my.cats.advanced

import my.wrapper.Wrapper

object MonoidCatsApp {

  def monoid(): Unit = {
    import cats.Monoid
    //!! this is needed, otherwise Monoid[String].combine("Hi ", "there") fails
    import cats.instances.string._

    Monoid[String].combine("Hi ", "there")
    // res0: String = Hi there
    Monoid[String].empty
    // res1: String = ""

    /**
     * The above is equivalent to below:
     *
     * (from 1.4 Summary of Advanced Scala with Cats,
     *   >Each type class has a companion object with
     *   >an apply method for materializing instances;
     * )
     */

    Monoid.apply[String].combine("Hi ", "there")
    // res2: String = Hi there
    Monoid.apply[String].empty
    // res3: String = "
  }

  def semiGroup(): Unit = {
    import cats.Semigroup
    import cats.instances.string._

    Semigroup[String].combine("Hi ", "there")
    // res4: String = Hi there
  }

  def defaultTypeClassInstances(): Unit = {
    import cats.Monoid
    //The type class instances for Monoid are organised under cats.instances
    import cats.instances.int._
    Monoid[Int].combine(32, 10)
    // res5: Int = 42

    val a = Option(22)
    // a: Option[Int] = Some(22)
    val b = Option(20)
    // b: Option[Int] = Some(20)

    import cats.instances.option._
    Monoid[Option[Int]].combine(a, b)
    // res6: Option[Int] = Some(42)
  }

  def combineOp(): Unit ={
    import cats.Monoid
    import cats.syntax.semigroup._ //this is needed, otherwise, `value |+| is not a member of ...`
    /**
     * trait SemigroupSyntax {
     *   // TODO: use simulacrum instances eventually
     *   implicit def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
     *     new SemigroupOps[A](a)
     * }
     *
     * final class SemigroupOps[A: Semigroup](lhs: A) {
     *   def |+|(rhs: A): A = macro Ops.binop[A, A]      // *** <- |+| op ***
     *   def combine(rhs: A): A = macro Ops.binop[A, A]
     *   def combineN(rhs: Int): A = macro Ops.binop[A, A]
     * }
     */
    import cats.instances.string._ //this is also needed, otherwise the same error and
    //could not find implicit value for parameter ev: cats.kernel.Monoid[String]
    /**
     * trait StringInstances extends cats.kernel.instances.StringInstances {
     *   implicit val catsStdShowForString: Show[String] =
     *     Show.fromToString[String]
     * }
     *
     * trait StringInstances {
     *   implicit val catsKernelStdOrderForString: Order[String] = new StringOrder
     *   implicit val catsKernelStdMonoidForString: Monoid[String] = new StringMonoid // *** <- Monoid!! ***
     * }
     */

    val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
    // stringResult: String = Hi there

    import cats.instances.int._
    val intResult = 1 |+| 2 |+| Monoid[Int].empty
    // intResult: Int = 3
  }

  def exercise1(): Unit = {
    def add1(items: List[Int]): Int =
      items.foldLeft(0)(_ + _)

    /**
     * Or
     */

    import cats.Monoid
    import cats.instances.int._
    import cats.syntax.semigroup._
    def add2(items: List[Int]): Int =
      items.foldLeft(Monoid[Int].empty)(_ |+| _) //init value = Monoid.empty, operation = Monoid.combine (i.e. |+|)
  }

  def exercise2() {
    import cats.Monoid
    import cats.syntax.semigroup._
    import cats.instances.int._
    import cats.instances.option._

    //Now there is a use case for Monoids. We need a single method that adds Ints and instances of Option[Int].
    def add3[A](items: List[A])(implicit monoid: Monoid[A]): A =
      items.foldLeft(monoid.empty)(_ |+| _)

    add3(List(1,3))
    add3(List(Option(1), Option(2)))

    //!! very weird!! this does not compile
    //add3(List(Some(1), Some(2)))

    //... but this is ok .... a bug in Cats?
    add3(List(Some(1), Option(2)))

    println(add3(List(Some(1), None)))

    /**
     * We can op onally use Scala’s context bound syntax to write the same code in a friendlier way:
     * 
     * See the difference in type declaration [A: Monoid]
     */
    def add4[A: Monoid](items: List[A]): A =
      items.foldLeft(Monoid[A].empty)(_ |+| _)

    add4(List(1,3))
    add4(List(Option(1), Option(2)))

    //!! very weird!! this does not compile
    //add4(List(Some(1), Some(2)))

    //... but this is ok .... a bug in Cats?
    add4(List(Some(1), Option(2)))
    //Probably not a bug ... maybe due to type variance and Cats being in favor of invariant type classes
    //See the associated chapter(s) in the Advanced Scala book

    println(add4(List(Some(1), None)))

    add4(List(Some(1), None, Some(2), None, Some(3)))
    // res10: Option[Int] = Some(6)
  }

  def typeVariance(): Unit = {
    //import cats.instances.option._
    import cats.syntax.option._

    Some(1)
    // res0: Some[Int] = Some(1)

    1.some
    // res1: Option[Int] = Some(1)

    None
    // res2: None.type = None

    none[Int]
    // res3: Option[Int] = None
  }

  def identicallyTyped(): Unit = {
    import cats.Monoid
    import cats.instances.int._
    import cats.syntax.semigroup._

    //multiple type class instances when several are available for a specific type
    //
    //We can always define or import a type class instance into the local scope.
    //his will take precedence over other type class instances in the implicit scope:
    implicit val multiplicationMonoid =
      new Monoid[Int] {
        def empty: Int = 1
        override def combine(x: Int, y: Int): Int = x * y
      }

    // multiplicationMonoid takes precedence over cats.instances.int
    // weird, this doesn't compile...
    //println(3 |+| 2)
    // res5: Int = 6 // not 5
  }

  def main(args: Array[String]): Unit = {
    // "One" ++ "two" // No ++ for String :|
    Wrapper("monoid")(monoid)
    Wrapper("semiGroup")(semiGroup)
    Wrapper("exercise1")(exercise1)
    Wrapper("exercise2")(exercise2)
    Wrapper("identicallyTyped")(identicallyTyped)
  }

}
