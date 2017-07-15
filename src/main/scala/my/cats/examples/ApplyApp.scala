package my.cats.examples

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/apply
 *
 * Apply extends the Functor type class (which features the familiar map function) with a new function ap.
 *
 *   trait Apply[F[_]] extends Functor[F] with ... {
 *     ...
 *     def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
 *     ...
 *   }
 *
 * The difference between ap and map is that
 * for ap the function that takes care of the transformation is of type F[A => B], whereas for map it is A => B
 */
object ApplyApp {
  import cats._
  import cats.implicits._

  def applyInstancesTest(): Unit ={
    implicit val optionApply: Apply[Option] = new Apply[Option] {
      def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
        fa.flatMap(a => f.map(ff => ff(a)))

      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        fa.flatMap(a => fb.map(b => (a, b)))
    }

    // Some(6)
    println(
      optionApply.ap(
        Some((s:String) => s.length)
      )(
        Some("abcdef")
      )
    )

    implicit val listApply: Apply[List] = new Apply[List] {
      def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
        fa.flatMap(a => f.map(ff => ff(a)))

      def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
        fa.zip(fb)
    }

    // List(6, 3, 4)
    println(
      listApply.ap(
        List((s:String) => s.length)
      )(
        List("abcdef", "ghi", "jklm")
      )
    )
  }

  /**
   * Since Apply extends Functor, we can use the map method from Functor
   */
  def mapTest(): Unit = {
    val intToString: Int ⇒ String = _.toString
    val double: Int ⇒ Int = _ * 2
    val addTwo: Int ⇒ Int = _ + 2

    // Some("1")
    println(Apply[Option].map(Some(1))(intToString))

    // Some(2)
    println(Apply[Option].map(Some(1))(double))

    // None
    println(Apply[Option].map(None)(addTwo))
  }

  /**
   * And like functors, Apply instances also compose
   */
  def composeTest(): Unit ={
    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) ⇒ x + 1

    // List(Some(2), None, Some(4))
    println(
      listOpt.ap(
        List(Some(plusOne))
      )(
        List(Some(1), None, Some(3))
      )
    )
  }


  /**
   * The ap method is a method that Functor does not have:
   *
   *   def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
   */
  def apTest(): Unit = {
    val intToString: Int ⇒ String = _.toString
    val double: Int ⇒ Int = _ * 2

    //Some("1")
    println(Apply[Option].ap(Some(intToString))(Some(1)))

    //Some(2)
    println(Apply[Option].ap(Some(double))(Some(1)))

    //None
    println(Apply[Option].ap(Some(double))(None))

    //None
    println(Apply[Option].ap(None)(Some(1)))

    //None
    println(Apply[Option].ap(None)(None))
  }

  def ap2Test(): Unit ={
    val addArity2 = (a: Int, b: Int) ⇒ a + b

    // Some(3)
    println(Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)))

    // *** None ***
    println(Apply[Option].ap2(Some(addArity2))(Some(1), None))
    
    val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

    // Some(3)
    println(Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)))
  }

  def map2Test(): Unit ={
    val addArity2 = (a: Int, b: Int) ⇒ a + b

    // Some(3)
    println(Apply[Option].map2(Some(1), Some(2))(addArity2))

    val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

    // Some(6)
    println(Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3))
  }

  def tuple2Test(): Unit ={
    val addArity2 = (a: Int, b: Int) ⇒ a + b

    // Some((1,2))
    println(Apply[Option].tuple2(Some(1), Some(2)))

    // Some((1,2,3))
    println(Apply[Option].tuple3(Some(1), Some(2), Some(3)))
  }

  def main(args: Array[String]): Unit = {
    Wrap("applyInstancesTest")(applyInstancesTest)
    Wrap("mapTest")(mapTest)
    Wrap("composeTest")(composeTest)
    Wrap("apTest")(apTest)
    Wrap("ap2Test")(ap2Test)
    Wrap("map2Test")(map2Test)
    Wrap("tuple2Test")(tuple2Test)
  }
}
