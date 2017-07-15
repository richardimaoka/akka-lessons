package my.cats

import my.wrapper.Wrap

/**
 * https://www.scala-exercises.org/cats/foldable
 */
object FoldableApp {
  import cats._
  import cats.implicits._

  def foldLeftTest(): Unit ={
    //6
    println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _))

    //abc
    println(Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _))
  }

  /**
   * foldRight is a ***lazy*** right-associative fold on F using the given function.
   * The function has the signature (A, Eval[B]) => Eval[B] to support laziness in a stack-safe way.
   */
  def foldRightTest(): Unit ={
    val lazyResult =
      Foldable[List]
        // the initial value is in Eval[B], due to f: (A, Eval[B]) => Eval[B]
        .foldRight(List(1, 2, 3), Now(0))(
          // (A, Eval[B]) => Eval[B]
          (x, rest) ⇒ Later(x + rest.value)
        )

    // 6
    println(lazyResult.value) //without .value, it doesn't calculate the value yet
  }

  /**
   * fold, also called combineAll
   * combines every value in the foldable using the given Monoid instance.
   *
   *   def fold[A](fa: F[A])(implicit A: Monoid[A]): A
   */
  def foldTest(): Unit ={
    //abc
    println(Foldable[List].fold(List("a", "b", "c")))

    //6
    println(Foldable[List].fold(List(1, 2, 3)))
  }

  def foldMapTest(): Unit ={
    //9
    println(Foldable[List].foldMap(List("a", "b", "c"))(_.length))

    //"123"
    println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))
  }

  /**
   * foldK is similar to fold but combines every value
   * in the foldable using the given MonoidK[G] instance instead of Monoid[G].
   */
  def foldkTest(): Unit ={
    //List(1, 2, 3, 4, 5)
    println(Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))))

    //Some(two)
    println(Foldable[List].foldK(List(None, Option("two"), Option("three"))))
  }

  /**
   *  def find[A](fa: F[A])(f: A => Boolean): Option[A]
   */
  def findTest(): Unit ={
    // Some(3)
    println(Foldable[List].find(List(1, 2, 3))(_ > 2))

    // None
    println(Foldable[List].find(List(1, 2, 3))(_ > 5))
  }

  /**
   * def exists[A](fa: F[A])(p: A => Boolean): Boolean
   */
  def existsTest(): Unit ={
    // true
    println(Foldable[List].exists(List(1, 2, 3))(_ > 2))

    // false
    println(Foldable[List].exists(List(1, 2, 3))(_ > 5))
  }

  /**
   * forall checks whether all elements satisfy the predicate
   *   def find[A](fa: F[A])(f: A => Boolean): Option[A]
   */
  def forallTest(): Unit ={
    println(Foldable[List].forall(List(1, 2, 3))(_ <= 3))

    println(Foldable[List].forall(List(1, 2, 3))(_ < 3))
  }

  /**
   * Convert F[A] to List[A].
   */
  def toListTest(): Unit ={
    println(Foldable[List].toList(List(1, 2, 3)))

    println(Foldable[Option].toList(Option(42)))

    println(Foldable[Option].toList(None))
  }

  /**
   * Convert F[A] to List[A] only including the elements that match a predicate.
   */
  def filterTest(): Unit ={
    println(Foldable[List].filter_(List(1, 2, 3))(_ < 3))

    println(Foldable[Option].filter_(Option(42))(_ != 42))
  }

  def traverseTest(): Unit ={
    def parseInt(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption

    // Some(())
    println(Foldable[List].traverse_(List("1", "2", "3"))(parseInt))

    // None
    println(Foldable[List].traverse_(List("a", "b", "c"))(parseInt))
  }

  def composeTest(): Unit ={
    val FoldableListOption = Foldable[List].compose[Option]

    // 10
    println(FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))))

    // "123"
    println(FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))))
  }

  def moreFoldableMethodsTest(): Unit ={
    // false
    println(Foldable[List].isEmpty(List(1, 2, 3)))

    // List(2, 3)
    println(Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2))

    // List(1)
    println(Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2))
  }

  def main(args: Array[String]): Unit = {
    Wrap("foldLeftTest")(foldLeftTest)
    Wrap("foldRightTest")(foldRightTest)
    Wrap("foldTest")(foldTest)
    Wrap("foldMapTest")(foldMapTest)
    Wrap("foldkTest")(foldkTest)
    Wrap("findTest")(findTest)
    Wrap("existsTest")(existsTest)
    Wrap("forallTest")(forallTest)
    Wrap("toListTest")(toListTest)
    Wrap("filterTest")(filterTest)
    Wrap("traverseTest")(traverseTest)
    Wrap("composeTest")(composeTest)
    Wrap("moreFoldableMethodsTest")(moreFoldableMethodsTest)
  }
}
