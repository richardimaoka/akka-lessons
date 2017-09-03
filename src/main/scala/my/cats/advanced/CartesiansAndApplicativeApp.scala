package my.cats.advanced

import my.wrapper.Wrap

object CartesiansAndApplicativeApp {
  def first(): Unit ={
    import cats.syntax.either._
    def parseInt(str: String): Either[String, Int] =
      Either.catchOnly[NumberFormatException](str.toInt).
        leftMap(_ => s"Couldn't read $str")

    val result = for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield (a + b + c)

    println(result)
    // res1: scala.util.Either[String,Int] = Left(Couldn't read a)
  }

  def firstFuture(): Unit ={
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    lazy val timestamp0 = System.currentTimeMillis

    def getTimestamp: Long = {
      val timestamp = System.currentTimeMillis - timestamp0
      Thread.sleep(100)
      timestamp
    }

    val timestamps = for {
      a <- Future(getTimestamp)
      b <- Future(getTimestamp)
      c <- Future(getTimestamp)
    } yield (a, b, c)

    println(Await.result(timestamps, 1.second))
    // res5: (Long, Long, Long) = (0,106,210)
  }

  def cartesianExamples(): Unit ={
    import cats.Cartesian
    import cats.instances.option._ // Cartesian for Option

    Cartesian[Option].product(Some(123), Some("abc"))
    // res0: Option[(Int, String)] = Some((123,abc))

    Cartesian.tuple3(Option(1), Option(2), Option(3))
    // res3: Option[(Int, Int, Int)] = Some((1,2,3))

    /**
     * If either parameter evaluates to None, the enঞre result is None
     */
    Cartesian[Option].product(None, Some("abc"))
    // res1: Option[(Nothing, String)] = None
    Cartesian[Option].product(Some(123), None)
    // res2: Option[(Int, Nothing)] = None

    Cartesian.tuple3(Option(1), Option(2), Option.empty[Int])
    // res4: Option[(Int, Int, Int)] = None

    Cartesian.map3(
      Option(1),
      Option(2),
      Option(3)
    )(_ + _ + _)
    // res5: Option[Int] = Some(6)

    Cartesian.map3(
      Option(1),
      Option(2),
      Option.empty[Int]
    )(_ + _ + _)
    // res6: Option[Int] = None
  }

  def cartesianBuilder(): Unit ={
    import cats.instances.option._
    import cats.syntax.cartesian._

    (Option(123) |@| Option("abc")).tupled
    // res7: Option[(Int, String)] = Some((123,abc))

    val builder2 = Option(123) |@| Option("abc")
    builder2.tupled
    // res8: Option[(Int, String)] = Some((123,abc))

    val builder3 = Option(123) |@| Option("abc") |@| Option(true)
    builder3.tupled
    // res9: Option[(Int, String, Boolean)] = Some((123,abc,true))

    val builder5 = builder3 |@| Option(0.5) |@| Option('x')
    builder5.tupled
    // res10: Option[(Int, String, Boolean, Double, Char)] = Some((123,abc,true,0.5,x))

    /**
     * The idiomaঞc way of wriঞng builder syntax
     */
    (
      Option(1) |@|
        Option(2) |@|
        Option(3)
      ).tupled
    // res11: Option[(Int, Int, Int)] = Some((1,2,3))

    case class Cat(name: String, born: Int, color: String)

    (
      Option("Garfield") |@|
        Option(1978) |@|
        Option("Orange and black")
      ).map(Cat.apply)
    // res12: Option[Cat] = Some(Cat(Garfield,1978,Orange and black))

    /**
     * If we supply a funcঞon that accepts the wrong number or types of
     * parameters, we get a compile error:
     */
    val add: (Int, Int) => Int = (a, b) => a + b
    // add: (Int, Int) => Int = <function2>

    //(Option(1) |@| Option(2) |@| Option(3)).map(add)
    // <console>:27: error: type mismatch;
    // found : (Int, Int) => Int
    // required: (Int, Int, Int) => ?
    // (Option(1) |@| Option(2) |@| Option(3)).map(add)
    //                                             ^

    //(Option("cats") |@| Option(true)).map(add)
    // <console>:27: error: type mismatch;
    // found : (Int, Int) => Int
    // required: (String, Boolean) => ?
    // (Option("cats") |@| Option(true)).map(add)
    // ^
  }

  def fancyFunctors(): Unit ={
    import cats.Monoid
    import cats.instances.boolean._
    import cats.instances.int._
    import cats.instances.list._
    import cats.instances.string._
    import cats.syntax.cartesian._
    import cats.syntax.apply._
    /**
     * Cartesian builders also have a contramap and imap methods that accept
     * Contravariant and Invariant functors.
     */
    case class Cat(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
    )

    def catToTuple(cat: Cat) =
      (cat.name, cat.yearOfBirth, cat.favoriteFoods)

//    /**
//     * def imap[Z](f: (A0, A1, A2) => Z)(g: Z => (A0, A1, A2))
//     * (implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[Z]
//     *   = Cartesian.imap3(a0, a1, a2)(f)(g)
//     */
//    implicit val catMonoid = (
//      Monoid[String],
//        Monoid[Int],
//        Monoid[List[String]]
//      ).imap(Cat.apply)(catToTuple(_))
//
//    println(catMonoid)
//    //cats.KernelInvariantMonoidalInstances$$anon$3$$anon$5@7ef36dae
  }

  def main(args: Array[String]): Unit ={
    Wrap("first")(first)
    Wrap("firstFuture")(firstFuture)
    Wrap("cartesianExamples")(cartesianExamples)
    Wrap("fancyFunctors")(fancyFunctors)
  }
}
