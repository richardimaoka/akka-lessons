package my.cats

import cats.Functor
import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/functor
 *
 * A Functor is a ubiquitous type class involving types that have one "hole", i.e. types which have the shape F[?]
 * The Functor category involves a single operation, named map:
 *
 *   def map[A, B](fa: F[A])(f: A => B): F[B]
 */
object FunctorApp {
  import cats._
  import cats.implicits._

  def mapMethodOfScalaCollectionTest(): Unit = {
    /**
     * The name of the method map should remind you of the map method that exists on
     * many classes in the Scala standard library, for example:
     *
     *   def map[B](f: A => B): Option[B] //(i.e.) this is not Functor's method but Collection's method
     */
    // Some(2)
    println(Option(1).map(_ + 1))

    /**
     *   def map[B](f: A => B): List[B] //(i.e.) this is not Functor's method but Collection's method
     */
    // List(2,3,4)
    println(List(1, 2, 3).map(_ + 1))

    /**
     *   def map[B](f: A => B): Vector[B] //(i.e.) this is not Functor's method but Collection's method
     */
    // Vector("1", "2", "3")
    println(Vector(1, 2, 3).map(_.toString))
  }

  def createFunctors(): Unit ={
    /**
     * Functor has the following map method,
     *
     *   def map[A, B](fa: F[A])(f: A => B): F[B]
     *
     * and the below *instances* are for specific F[]'s
     */
    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B) = fa map f
    }

    implicit val listFunctor: Functor[List] = new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B) = fa map f
    }

//    implicit def function1Functor[In]: Functor[Function1[In, ?]] =
//      new Functor[Function1[In, ?]] {
//        def map[A, B](fa: In => A)(f: A => B): Function1[In, B] = fa andThen f
//      }
  }

  def listFunctor(): Unit = {
    println(Functor[List].map(List("qwer", "adsfg"))(_.length))
  }

  def listTest(): Unit = {
    /**
     * _.length is defined for String, not for Option[String]
     * We can use Functor to "lift" a function from A => B to F[A] => F[B]
     *
     *   def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
     *
     *   def map[A, B](fa: F[A])(f: A => B): F[B]
     */
    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    //                                           = (o: Option[String]) => o.map(_.length)

    //Some(4)
    println(lenOption(Some("abcd")))
  }

  def fproductTest(): Unit = {
    /**
     * Functor provides an fproduct function which pairs a value
     * with the result of applying a function to that value.
     *
     *   def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)]
     *     = map(fa)(a => a -> f(a))
     */
    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    /**
     * Map(input -> output)
     *   the input of _.length  (e.g.) "Cats"
     *   the output of _.length (e.g.) 4
     */
    // Map(Cats -> 4, is -> 2, awesome -> 7)
    println(product)

    // 4
    println(product.get("Cats").getOrElse(0))

    // 2
    println(product.get("is").getOrElse(0))

    // 7
    println(product.get("awesome").getOrElse(0))
  }

  def composeTest(): Unit = {
    val listOpt = Functor[List] compose Functor[Option]

    // List(Some(2), None, Some(4))
    println(listOpt.map(List(Some(1), None, Some(3)))(_ + 1))
  }

  def main(args: Array[String]): Unit = {
    Wrap("mapMethodOfScalaCollectionTest")(mapMethodOfScalaCollectionTest)
    Wrap("createFunctors")(createFunctors)
    Wrap("listFunctor")(listFunctor)
    Wrap("listTest")(listTest)
    Wrap("fproductTest")(fproductTest)
    Wrap("composeTest")(composeTest)
  }
}
