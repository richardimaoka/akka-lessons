package my.cats.herding.day0

import my.wrapper.Wrap

object Polymorphism {

  def parametricPolymorphismTest(): Unit ={
    def head[A](xs: List[A]): A = xs(0)

    // 1
    println(head(1 :: 2 :: Nil))

    case class Car(make: String)

    // Car(Civic)
    println(head(Car("Civic") :: Car("CR-V") :: Nil))
  }

  def subtypePolymorphismTest(): Unit ={
    def plus[A](a1: A, a2: A): A = ???

    /**
     * plus takes only one parameters, hence dependency on `this`
     */
    trait PlusIntf[A] {
      def plus(a2: A): A
    }

    class IntPlus(a1: Int) extends PlusIntf[Int] {
      def plus(a2: Int): Int = this.a1 + a2
    }

    def plusBySubtype[A <: PlusIntf[A]](a1: A, a2: A): A = a1.plus(a2)

    val a1: IntPlus = new IntPlus(10)
    val a2: IntPlus = new IntPlus(100)

    //println( plusBySubtype(a1, a2))
    /**
     * [error] /Users/yunishiyama/akka-lessons/src/main/scala/my/cats/herding/day0/Polymorphism.scala:37: inferred type arguments [IntPlus] do not conform to method plusBySubtype's type parameter bounds [A <: PlusIntf[A]]
     * [error]     println( plusBySubtype(a1, a2))
     * [error]              ^
     * [error] /Users/yunishiyama/akka-lessons/src/main/scala/my/cats/herding/day0/Polymorphism.scala:37: type mismatch;
     * [error]  found   : IntPlus
     * [error]  required: A
     * [error]     println( plusBySubtype(a1, a2))
     * [error]                            ^
     * [error] /Users/yunishiyama/akka-lessons/src/main/scala/my/cats/herding/day0/Polymorphism.scala:37: type mismatch;
     * [error]  found   : IntPlus
     * [error]  required: A
     * [error]     println( plusBySubtype(a1, a2))
     */
  }

  def adhocPolymorphismTest(): Unit = {
    /**
     * plus takes two parameters, hence NO dependency on `this`
     */
    trait CanPlus[A] {
      def plus(a1: A, a2: A): A
    }

    def plus[A: CanPlus](a1: A, a2: A): A = implicitly[CanPlus[A]].plus(a1, a2)
  }

  def sumTest(): Unit ={
    def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

    println(sum(List(1, 2, 3, 4)))
  }

  def sumByIntMonoidTest(): Unit ={
    object IntMonoid {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    def sum(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)

    println(sum(List(1, 2, 3, 4)))
  }

  def monoidTest(): Unit ={
    //type class
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    //type class *instance*
    object IntMonoid extends Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    def sum(xs: List[Int], m: Monoid[Int]): Int = xs.foldLeft(m.mzero)(m.mappend)

    sum(List(1, 2, 3, 4), IntMonoid)
  }

  def monoidTestImplicit(): Unit = {
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    /**
     * Without defining IntMonad, you will get an error like below:
     *
     *   [error] Polymorphism.scala:109: could not find implicit value for parameter m: Monoid[Int]
     *   [error]     println(sum(List(1, 2, 3, 4)))
     *   [error]                ^
     *   [error] one error found
     */
    implicit object IntMonoid extends Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

    println(sum(List(1, 2, 3, 4)))


    implicit object DoubleMonoid extends Monoid[Double] {
      def mappend(a: Double, b: Double): Double = a + b
      def mzero: Double = 0
    }

    println(sum(List(1.2, 2.3, 3.09, 4.8)))


    implicit object StringMonoid extends Monoid[String] {
      def mappend(a: String, b: String): String = a + b
      def mzero: String = ""
    }

    println(sum(List("abc", "def", "ghi")))

  }

  def contextBoundTest(): Unit = {
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    /**
     * Using `implicitly` method.
     *
     *   compare with:
     *    def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
     *      xs.foldLeft(m.mzero)(m.mappend) // <- implementation is same
     */
    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }

    implicit object IntMonoid extends Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    sum(List(1, 2, 3, 4))
  }

  def insideCompanionTest(): Unit ={
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    object Monoid {
      /**
       * Scala’s implicit resolution rules: When it needs an implicit parameter of some type,
       * it’ll look for anything in scope.
       * It’ll include the companion object of the type that you’re looking for.
       */
      implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }
      implicit val StringMonoid: Monoid[String] = new Monoid[String] {
        def mappend(a: String, b: String): String = a + b
        def mzero: String = ""
      }
    }

    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }
    // works same for the below:
    //def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

    println(sum(List(1, 2, 3, 4)))
  }

  def multiMonoidTest(): Unit ={
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    /**
     * Interesting, this also accepts explicitly passing a parameter, same as:
     *  def sum[A](xs: List[A])(implicit m: Monoid[A]): A
     */
    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }
    /**
     *  Aaaahh, according to http://eed3si9n.com/herding-cats/ja/sum-function.html
     *  defining the function like this will actually add the implicit parameter
     *
     *  // on console
     *  scala> def sum[A: Monoid](xs: List[A]): A = {
     *    val m = implicitly[Monoid[A]]
     *    xs.foldLeft(m.mzero)(m.mappend)
     *  }
     *  sum: [A](xs: List[A])(implicit evidence$1: Monoid[A])A
     *
     *
     *  So, this:
     *    def sum[A: Monoid](xs: List[A]): A = {
     *      val m = implicitly[Monoid[A]]
     *      xs.foldLeft(m.mzero)(m.mappend)
     *    }
     *
     *  and this:
     *    def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)
     *
     *  are equivalent.
     */

    println(sum(List(1, 2, 3, 4)))

    val multiplyMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a * b
      def mzero: Int = 1
    }

    println(sum(List(1, 2, 3, 4))(multiplyMonoid))
  }

  def main(args: Array[String]): Unit = {
    Wrap("parametricPolymorphismTest")(parametricPolymorphismTest)
    Wrap("subtypePolymorphismTest")(subtypePolymorphismTest)
    Wrap("sumTest")(sumTest)
    Wrap("sumByIntMonoidTest")(sumByIntMonoidTest)
    Wrap("monoidTestImplicit")(monoidTestImplicit)
    Wrap("insideCompanionTest")(insideCompanionTest)
    Wrap("multiMonoidTest")(multiMonoidTest)
  }
}
