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

    //This is ok .... a bug in Cats?
    add3(List(Some(1), Option(2)))

    println(add3(List(Some(1), None)))
  }

  def main(args: Array[String]): Unit = {
    // "One" ++ "two" // No ++ for String :|
    Wrapper("monoid")(monoid)
    Wrapper("semiGroup")(semiGroup)
    Wrapper("exercise1")(exercise1)
  }

}
