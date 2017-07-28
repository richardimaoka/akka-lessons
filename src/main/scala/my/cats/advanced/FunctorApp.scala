package my.cats.advanced

import my.wrapper.Wrap

object FunctorApp {

  def mapTest(): Unit ={
    /**
     * Informally, a functor is anything with a map method.
     */
    List(1, 2, 3).map(x => (x % 2) == 0)
    // res0: List[Boolean] = List(false, true, false)

    List(1, 2, 3).map(_ * 2).map(_ + 4)
    // res1: List[Int] = List(6, 8, 10)

    List(1, 2, 3).map(x => (x * 2) + 4)
    // res2: List[Int] = List(6, 8, 10)

    Option(1).map(_.toString)
    // res3: Option[String] = Some(1)

    Option(123).map(_ * 4).map(_ + 4)
    // res4: Option[Int] = Some(496)

    Option(123).map(x => (x * 2) + 4)
    // res5: Option[Int] = Some(250)
  }

  def moreConcreteExamples(): Unit ={
    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val future1 = Future("Hello world!")
    // future1: scala.concurrent.Future[String] = Future(<not completed>)

    val future2 = future1.map(_.length)
    // future2: scala.concurrent.Future[Int] = Future(<not completed>)

    Await.result(future1, 1.second)
    // res6: String = Hello world!

    Await.result(future2, 1.second)
    // res7: Int = 12
  }

  def functionMaps(): Unit ={
    import cats.instances.function._
    import cats.syntax.functor._

    val func1 = (x: Int)    => x.toDouble
    // func1: Int => Double = <function1>

    val func2 = (y: Double) => (y * 2).toString
    // func2: Double => Double = <function1>

    val func3 = func1.map(func2)
    // func3: Int => String = scala.runtime.AbstractFunction1$$Lambda$9405/272413009@2deb99ee

    func3(1) // function composition by calling map
    // res8: String = 2.0

    func2(func1(1)) // function composition written out by hand
    // res9: String = 2.0

  }

  def functorTest(): Unit ={
    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._

    val list1 = List(1, 2, 3)
    // list1: List[Int] = List(1, 2, 3)

    /**
     * See Functor[List]
     *
     * def map[A, B](fa: F[A])(f: A => B): F[B]
     */
    val list2 = Functor[List].map(list1)(_ * 2)
    // list2: List[Int] = List(2, 4, 6)

    val option1 = Option(123)
    // option1: Option[Int] = Some(123)

    val option2 = Functor[Option].map(option1)(_.toString)
    // option2: Option[String] = Some(123)
  }

  def listTest(): Unit = {
    import cats.Functor
    import cats.instances.option._

    val func = (x: Int) => x + 1
    // func: Int => Int = <function1>

    /**
     * def lift[A, B](f: A => B): F[A] => F[B]
     *   = map(_)(f)
     *
     * (i.e.) lifted is a function to convert from F[A] to F[B]
     * in this case,
     *  Option[A] to Option[B]
     */
    val lifted = Functor[Option].lift(func)

    // lifted: Option[Int] => Option[Int] = cats.Functor$$Lambda$28362/1686307543@514a39b6

    lifted(Option(1))
    // res0: Option[Int] = Some(2)
  }

  def syntaxTest(): Unit = {
    import cats.instances.function._
    import cats.syntax.functor._

    val func1 = (a: Int) => a + 1
    // func1: Int => Int = <function1>

    val func2 = (a: Int) => a * 2
    // func2: Int => Int = <function1>

    val func3 = func1.map(func2)
    // func3: Int => Int = scala.runtime.
    //AbstractFunction1$$Lambda$9405/272413009@5f430fe0

    func3(123)
    // res1: Int = 248

    /**
     * Better example?
     */

    val func4: Int => String =
      (a: Int) => s"${a} + ${a} = ${a + a}"

    val func5: String => Option[String] =
      (a: String) => Some(a)

    val func45 = func4.map(func5)
    //Some(16 + 16 = 32)

    println(func45)
    println(func45(16))

  }

  def syntaxTest2(): Unit = {
    import cats.instances.function._
    import cats.syntax.functor._

    /**
     * In other words, “mapping” over a Function1 is just func on composion:
     */
    val func1 = (x: Int)    => x.toDouble
    // func1: Int => Double = <function1>

    val func2 = (y: Double) => y * 2
    // func2: Double => Double = <function1>

    val func3 = func1.map(func2)
    // func3: Int => Double = scala.runtime.
    //AbstractFunction1$$Lambda$9405/272413009@2deb99ee

    func3(1) // function composition by calling map
    // res8: Double = 2.0

    func2(func1(1)) // function composition written out by hand
    // res9: Double = 2.0
  }

  def catsFunctorTest(): Unit ={
    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._

    val list1 = List(1, 2, 3)
    // list1: List[Int] = List(1, 2, 3)

    val list2 = Functor[List].map(list1)(_ * 2)
    // list2: List[Int] = List(2, 4, 6)

    val option1 = Option(123)
    // option1: Option[Int] = Some(123)

    //F[Int] map(f: Int => String) = F[String]
    val option2 = Functor[Option].map(option1)(_.toString)
    // option2: Option[String] = Some(123)

    //F[Int] map(f: Int => String) = F[String]
    val list3 = Functor[List].map(List(1,2,3))(_.toString)

    val func = (x: Int) => x + 1
    // func: Int => Int = <function1>

    val lifted = Functor[Option].lift(func)
    // lifted: Option[Int] => Option[Int] = cats.
    //Functor$$Lambda$28362/1686307543@514a39b6

    lifted(Option(1))
    // res0: Option[Int] = Some(2)
  }

  def instancesForCustomTypes(): Unit = {
    import cats.Functor

    implicit val optionFunctor = new Functor[Option] {
      def map[A, B](value: Option[A])(func: A => B): Option[B] =
        value.map(func)
    }

    import scala.concurrent.{Future, ExecutionContext}

    /**
     * we need to inject dependencies into our instances.
     * For example, if we had to define a custom Functor for Future,
     * we would need to account for the implicit ExecutionContext parameter on future.map.
     * We can’t add extra parameters to functor.map so we have to account for the dependency
     * when we create the instance:
     */
    implicit def futureFunctor(implicit ec: ExecutionContext) =
      new Functor[Future] {
        def map[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
      }
  }

  def exercise1(): Unit = {
    import cats.Functor
    import cats.syntax.functor._

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]


    implicit val treeFunctor = new Functor[Tree] {
      def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
        case Branch(l,r) => Branch(map(l)(func),map(r)(func))
        case Leaf(v) => Leaf(func(v))
      }
    }

    //value map is not a member of Branch[Int]
    //Branch(Leaf(10), Leaf(20)).map(_ * 2)
    /**
     * Oops! This is the same invariance problem we saw with Monoids.
     * The compiler can’t find a Functor instance for Leaf.
     * Let’s add some smart constructors to compensate:
     */

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)

    /**
     * Now we can use our Functor properly:
     */
    leaf(100).map(_ * 2)
    // res6: Tree[Int] = Leaf(200)

    branch(leaf(10), leaf(20)).map(_ * 2)
    // res7: Tree[Int] = Branch(Leaf(20),Leaf(40))
  }

  def contraAndInvariant(): Unit = {
    Option(1).map(_ + 2).map(_ * 3).map(_ + 100)
    // res0: Option[Int] = Some(109)

    trait Printable[A] {
      def format(value: A): String

      /**
       * You can call `contramap` for an arbitrary type [B]
       */
      def contramap[B](func: B => A): Printable[B] =
        ???
    }

    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    case class MyType(a: Int)

    val my = MyType(10)

    val myPrintable = new Printable[MyType] {
      def format(value: MyType): String = s"MyType(a = ${value})"
      //def contramap[Option[String]](func: Option[String] => MyType)
    }
  }

  def contraAndInvariantExercise(): Unit = {
    import cats.syntax.functor._

    trait Printable[A] {
      def format(value: A): String
      def contramap[B](func: B => A): Printable[B] = {
        val self = this
        new Printable[B] {
          def format(value: B): String =
          self.format(func(value))
        }
      }
    }
    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    implicit val stringPrintable:  Printable[String] =
      new Printable[String] {
        def format(value: String): String =
          "\"" + value + "\""
      }

    implicit val booleanPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String =
          if(value) "yes" else "no"
      }

    format("hello")
    // res4: String = "hello"

    format(true)
    // res5: String = yes

    final case class Box[A](value: A)

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap[Box[A]](_.value)
      /**
       * def contramap[B](func: B => A): Printable[B]
       * Printable[A].contramap[B](func: B => A): Printable[B]
       *   where B = Box[A], so
       * Printable[A].contramap[Box[A]](func: Box[A] => A): Printable[Box[A]]
       *   and
       *   func: Box[A] => A = _.value
       */

    format(Box("hello world"))
    // res6: String = "hello world"

    format(Box(true))
    // res7: String = yes

    /**
     * If we don’t have a Printable for the contents of the Box,
     * calls to format should fail to compile:
     *
     *   could not find implicit value for parameter p: Printable[Box[Int]]
     */
    //format(Box(123))

    case class MyType(a: Int)

    val my = MyType(10)

    val myPrintable = new Printable[MyType] {
      def format(value: MyType): String = s"MyType(a = ${value})"
    }
  }

  def main(args: Array[String]): Unit = {
    Wrap("syntaxTest")(syntaxTest)
  }
}
