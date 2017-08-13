package my.cats.advanced

import my.wrapper.Wrap

object MonadApp {

  def optionsExamples(): Unit ={
    def parseInt(str: String): Option[Int] =
      scala.util.Try(str.toInt).toOption

    def divide(a: Int, b: Int): Option[Int] =
      if(b == 0) None else Some(a / b)

    println(s"""parseInt("1")  = ${parseInt("1")}""")
    //Some(1)
    println(s"""parseInt("2")  = ${parseInt("2")}""")
    //Some(1)
    println(s"""parseInt("-1") = ${parseInt("-1")}""")
    //Some(-1)
    println(s"""parseInt("a")  = ${parseInt("a")}""")
    //None

    println(s"""divide(1, 2)  = ${divide(1, 2)}""")
    //Some(0)
    println(s"""divide(3, 2)  = ${divide(3, 2)}""")
    //Some(1)
    println(s"""divide(0, 2)  = ${divide(0, 2)}""")
    //Some(0)
    println(s"""divide(5, 0)  = ${divide(5, 0)}""")
    //None

    def stringDivideBy(aStr: String, bStr: String): Option[Int] = {
      println(s"stringDivideBy called for aStr = $aStr, bStr = ${bStr}")
      println(s"trying parseInt(aStr = ${aStr})")
      parseInt(aStr).flatMap { aNum => {
        println(s"trying parseInt(bStr = ${bStr})")
        parseInt(bStr).flatMap { bNum => {
          println(s"trying divide(aNum = ${aNum}, bNum=${bNum})")
          divide(aNum, bNum)
        }}
      }}
    }

    //Fail at the last step on divide()
    println(stringDivideBy("1", "0"))
    //stringDivideBy called for aStr = 1, bStr = 0
    //trying parseInt(aStr = 1)
    //trying parseInt(bStr = 0)
    //trying divide(aNum = 1, bNum = 0)
    //None

    //Fail early when trying to parseInt("a")
    println(stringDivideBy("a", "0"))
    //stringDivideBy called for aStr = a, bStr = 0
    //trying parseInt(aStr = a)
    //None

    //Fail early when trying to parseInt("b")
    println(stringDivideBy("1", "b"))
    //stringDivideBy called for aStr = 1, bStr = b
    //trying parseInt(aStr = 1)
    //trying parseInt(bStr = b)
    //None

    /**
     * Every monad is also a functor,
     * so we can rely on both flatMap and map to sequence computations
     * that do and and don’t introduce a new monad.
     *
     * Plus, if we have both flatMap and map we can use for comprehensions
     * to clarify the sequencing behaviour:
     */
    def stringDivideBy2(aStr: String, bStr: String): Option[Int] =
      for {
        aNum <- {println(s"trying parseInt(aStr = ${aStr})"); parseInt(aStr)}
        bNum <- {println(s"trying parseInt(bStr = ${bStr})"); parseInt(bStr)}
        ans  <- {println(s"trying divide(aNum = ${aNum}, bNum = ${bNum})"); divide(aNum, bNum)}
      } yield ans

    println(s"""stringDivideBy2("6", "2") = ${stringDivideBy2("6", "2")}""")
    // trying parseInt(aStr = 6)
    // trying parseInt(bStr = 2)
    // trying divide(aNum = 6, bNum = 2)
    // stringDivideBy2("6", "2") = Some(3)

    println(s"""stringDivideBy2("6", "0") = ${stringDivideBy2("6", "0")}""")
    // trying parseInt(aStr = 6)
    // trying parseInt(bStr = 0)
    // trying divide(aNum = 6, bNum = 0)
    // stringDivideBy2("6", "0") = None

    //Fail early
    println(s"""stringDivideBy2("6", "foo") = ${stringDivideBy2("6", "foo")}""")
    // trying parseInt(aStr = 6)
    // trying parseInt(bStr = foo)
    // stringDivideBy2("6", "foo") = None

    //Fail early
    println(s"""stringDivideBy2("bar", "2") = ${stringDivideBy2("bar", "2")}""")
    // trying parseInt(aStr = bar)
    // stringDivideBy2("bar", "2") = None
  }

  def listMonads(): Unit = {
    /**
     * New mental model of `flatMap`
     *
     * function that return Lists === function with multiple return values
     *   => flatMap becomes a construct that calculates results
     *      from permutations ons and combinations of intermediate values.
     **/
    def numbersBetween(min: Int, max: Int): List[Int] =
      (min to max).toList

    /**
     * New mental model of `flatMap` cont'd:
     *
     * For example, in the for comprehension above, there are three possible
     * values of x and two possible values of y.
     * This means there are six possible values of the overall expression.
     * flatMap is genera ng these combinations from our code,
     * which simply says “get x from here and y from over there”.
     */
    val a = for {
      x <- numbersBetween(1, 3) //3 possible values (List elements) of x
      y <- numbersBetween(4, 5) //2 possible values (List elements) of y
    } yield (x, y)

    println(a)
    //List((1,4), (1,5), (2,4), (2,5), (3,4), (3,5))
  }

  def futureMonads(): Unit = {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    def getTrafficFromHost(hostname: String): Future[Int] =
      ??? // grab traffic information using a network client

    def getTrafficFromAllHosts: Future[Int] =
      for {
        traffic1 <- getTrafficFromHost("host1")
        traffic2 <- getTrafficFromHost("host2")
        traffic3 <- getTrafficFromHost("host3")
      } yield traffic1 + traffic2 + traffic3

    def getTrafficFromAllHostsFlatMap: Future[Int] =
      getTrafficFromHost("host1").flatMap { traffic1 =>
        getTrafficFromHost("host2").flatMap { traffic2 =>
          getTrafficFromHost("host3").map { traffic3 =>
            traffic1 + traffic2 + traffic3
          }
        } }
  }

  def monadTypeClasses(): Unit ={
    import cats.Monad
    import cats.instances.option._
    import cats.instances.list._

    val opt1 = Monad[Option].pure(3)
    // opt1: Option[Int] = Some(3)

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    // opt2: Option[Int] = Some(5)

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    // opt3: Option[Int] = Some(500)

    val list1 = Monad[List].pure(3)
    // list1: List[Int] = List(3)

    val list2 = Monad[List].
      flatMap(List(1, 2, 3))(x => List(x, x*10))
    // list2: List[Int] = List(1, 10, 2, 20, 3, 30)

    val list3 = Monad[List].map(list2)(_ + 123)
    // list3: List[Int] = List(124, 133, 125, 143, 126, 153)
  }

  def defaultInstances(): Unit = {
    import cats.Monad
    import cats.instances.option._

    Monad[Option].flatMap(Option(1))(x => Option(x*2))
    // res0: Option[Int] = Some(2)

    import cats.instances.list._
    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x*10))
    // res1: List[Int] = List(1, 10, 2, 20, 3, 30)
    /**
     * With the new mental model
     * For possible values x = 1, 2, 3
     * There are 3 possible values y = x, x*10
     * So in total, there are 6 values flattened
     */

    import cats.instances.vector._
    Monad[Vector].flatMap(Vector(1, 2, 3))(x => Vector(x, x*10))
    // res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)

    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._

    // val fm = Monad[Future]
    // <console>:37: error: could not find implicit value for
    // parameter instance: cats.Monad[scala.concurrent.Future]
    // val fm = Monad[Future]
    // ^

    import scala.concurrent.ExecutionContext.Implicits.global
    val fm = Monad[Future]
    // fm: cats.Monad[scala.concurrent.Future] = cats.instances.
    // FutureInstances$$anon$1@1c54d4ad

    /**
     * The below 3 forms are equivalent:
     */
    val result = Await.result(
      fm.flatMap(fm.pure(1)) { x =>
        fm.pure(x + 2)
      },
      1.second
    )
    println(result)

    val result2 = Await.result(
      fm.flatMap(Future(1)) { x => //it is like Future(1).flatMap( x => ...
        Future(x + 2)
      },
      1.second
    )
    println(result2)

    val result3 = Await.result(
      for {
        x <- fm.pure(1)
        y <- fm.pure(x + 2)
      } yield y,
      1.second
    )
    println(result3)
  }

  def monadSyntax(): Unit = {
    import cats.syntax.applicative._
    import cats.instances.option._
    import cats.instances.list._

    1.pure[Option]
    // res4: Option[Int] = Some(1)

    1.pure[List]
    // res5: List[Int] = List(1)

    import scala.language.higherKinds
    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      a.flatMap(x => b.map(y => x*x + y*y))

    import cats.instances.option._
    import cats.instances.list._

    sumSquare(Option(3), Option(4))
    // res8: Option[Int] = Some(25)

    sumSquare(List(1, 2, 3), List(4, 5))
    // res9: List[Int] = List(17, 26, 20, 29, 25, 34)

    /**
     *  We can rewrite this code using for comprehensions.
     */
    def sumSquareFor[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      for {
        x <- a
        y <- b
      } yield x*x + y*y

    sumSquareFor(Option(3), Option(4))
    // res10: Option[Int] = Some(25)

    sumSquare(List(1, 2, 3), List(4, 5))
    // res11: List[Int] = List(17, 26, 20, 29, 25, 34)
  }

  def identityMonad(): Unit = {
    import scala.language.higherKinds
    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      for {
        x <- a
        y <- b
      } yield x*x + y*y

    /**
     * This method works well on Options and Lists but we can’t call it
     * passing in plain values:
     */
    //sumSquare(3, 4)
    // <console>:22: error: no type parameters for method sumSquare:
    // (a: M[Int], b: M[Int])(implicit evidence$1: cats.Monad[M])M
    //   [Int] exist so that it can be applied to arguments (Int, Int
    // )
    // --- because ---
    // argument expression's type is not compatible with formal
    // parameter type;
    // found : Int
    // required: ?M[Int]
    // sumSquare(3, 4)
    // ^
    // <console>:22: error: type mismatch;
    // found : Int(3)
    // required: M[Int]
    // sumSquare(3, 4)
    // ^
    // <console>:22: error: type mismatch;
    // found : Int(4)
    // required: M[Int]
    // sumSquare(3, 4)
    // ^

    /**
     * It would be incredibly useful if we could use sumSquare with a
     * combination of monadic and non-monadic parameters.
     */
    type Id[A] = A
    println(sumSquare(3 : Id[Int], 4 : Id[Int]))
    // res2: cats.Id[Int] = 25

    val a = "Dave" : Id[String]
    println(a)
    // res3: cats.Id[String] = Dave

    val b = 123 : Id[Int]
    println(b)
    // res4: cats.Id[Int] = 123

    val c = List(1, 2, 3) : Id[List[Int]]
    println(c)
    // res5: cats.Id[List[Int]] = List(1, 2, 3)

    val aa = Monad[Id].pure(3)
    // a: cats.Id[Int] = 3
    val bb = Monad[Id].flatMap(aa)(_ + 1)
    // b: cats.Id[Int] = 4

    import cats.syntax.flatMap._
    import cats.syntax.functor._

    for {
      x <- aa
      y <- bb
    } yield x + y
    // res6: cats.Id[Int] = 7

    /**
     * The main use for Id is to write generic methods like sumSquare
     * that operate on monadic and non-monadic data types. For example,
     * we can run code asynchronously in production using Future and
     * synchronously in test using Id:
     */
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    // In production: async
    Await.result(sumSquare(Future(3), Future(4)), 1.second)
    // res8: Int = 25

    // In test: sync
    sumSquare(aa, bb)
    // res10: cats.Id[Int] = 25
  }

  def identityExercises(): Unit ={
    import cats.Id
    def pure[A](value: A): Id[A] =
      value

    pure(123)
    // res14: cats.Id[Int] = 123

    def map[A, B](initial: Id[A])(func: A => B): Id[B] =
      func(initial)

    map(123)(_ * 2)
    // res15: cats.Id[Int] = 246

    def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
      func(initial)
    // flatMap: [A, B](initial: cats.Id[A])(func: A => cats.Id[B])
    // cats.Id[B]

    flatMap(123)(_ * 2)
    // res16: cats.Id[Int] = 246

    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._
    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      for {
        x <- a
        y <- b
      } yield x*x + y*y

    sumSquare(3 : Id[Int], 4 : Id[Int])
  }

  def either(): Unit ={
    /**
     * In Scala 2.11, Either was unbiased. It had no map or flatMap method:
     * Scala 2.11 example (Hm? it compiles in 2.12)
     */
    Right(123).flatMap(x => Right(x * 2))
    // In Scala 2.11
    // <console>:12: error: value flatMap is not a member
    // of scala.util.Right[Nothing, Int]
    // Right(123).flatMap(x => Right(x * 2))
    // ^

    /**
     * Valid in Scala 2.11 and Scala 2.12
     * Instead of calling map or flatMap directly, we had to decide which side
     *  we wanted to be the “correct” side by taking a left- or right-projection
     */
    val either1: Either[String, Int] = Right(123)
    // either1: Either[String,Int] = Right(123)

    val either2: Either[String, Int] = Right(321)
    // either2: Either[String,Int] = Right(321)

    println(either1.right.flatMap(x => Right(x * 2)))
    // res2: scala.util.Either[String,Int] = Right(246)

    println(either2.left.flatMap(x => Left(x + "!!!")))
    // res3: scala.util.Either[String,Int] = Right(321)
    /**
     * This made the Scala 2.11 version of Either incovenient to use as a
     * monad. If we wanted to use for comprehensions, for example, we had
     * to insert calls to .right in every generator clause:
     */

    for {
      a <- either1.right
      b <- either2.right
    } yield a + b
    // res4: scala.util.Either[String,Int] = Right(444)

    /**
     * In Scala 2.12, Either was redesigned.
     * Either can be used as Monad
     */
    for {
      a <- either1
      b <- either2
    } yield a + b
    // res5: scala.util.Either[String,Int] = Right(444)

    import cats.syntax.either._
    val a = 3.asRight[String]
    // a: Either[String,Int] = Right(3)
    val b = 4.asRight[String]
    // b: Either[String,Int] = Right(4)
    val result = for {
      x <- a
      y <- b
    } yield x*x + y*y
    // res6: scala.util.Either[String,Int] = Right(25)

    println(result)

    /**
     * The asLeft and asRight methods have advantages over
     * Left.apply and Right.apply in terms of type inference. The
     * following code provides an example:
     **
     *def countPositive(nums: List[Int]) =
     *nums.foldLeft(Right(0)) { (accumulator, num) =>
     *if(num > 0) {
     *accumulator.map(_ + 1)
     *} else {
     *Left("Negative. Stopping!")
     *}
     * }
     **
     *1. the type of the accumulator ends up being Right instead of Either;
     *2. we didn’t specify type parameters for Right.apply so the compiler
     *infers the left parameter as Nothing
    */
    // <console>:18: error: type mismatch;
    // found   : scala.util.Either[Nothing,Int]
    // required: scala.util.Right[Nothing,Int]
    // accumulator.map(_ + 1)
    // ^
    // <console>:20: error: type mismatch;
    // found   : scala.util.Left[String,Nothing]
    // required: scala.util.Right[Nothing,Int]
    // Left("Negative. Stopping!")
    // ^

    def countPositive(nums: List[Int]) =
      // difference: 0.asRight - Switching to asRight avoids both of these problems.
      // It as a return type of Either
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if(num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    countPositive(List(1, 2, 3))
    // res7: Either[String,Int] = Right(3)
    countPositive(List(1, -2, 3))
    // res8: Either[String,Int] = Left(Negative. Stopping!)

    println(Either.catchOnly[NumberFormatException]("foo".toInt))
    // Left(java.lang.NumberFormatException: For input string: "foo")

    println(Either.catchNonFatal(sys.error("Badness")))
    // Left(java.lang.RuntimeException: Badness)

    println(Either.fromTry(scala.util.Try("foo".toInt)))
    // Left(java.lang.NumberFormatException: For input string: "foo")

    println(Either.fromOption[String, Int](None, "Badness"))
    // Left(Badness)

    /**
     * 4.4.3 Transforming Eithers
     */

    import cats.syntax.either._

    "Error".asLeft[Int].getOrElse(0)
    // res9: Int = 0

    "Error".asLeft[Int].orElse(2.asRight[String])
    // res10: Either[String,Int] = Right(2)

    -1.asRight[String].ensure("Must be non-negative!")(_ > 0)
    // res11: Either[String,Int] = Left(Must be non-negative!)

    val t1 = "error".asLeft[String] recover {
      case str: String =>
        "Recovered from " + str
    }
    println(s""""error".asLeft[String] = ${"error".asLeft[String]}""")
    println(s"t1: $t1")
    // res12: Either[String,String] = Right(Recovered from error)

    val t2 = "error".asLeft[String] recoverWith {
      case str: String =>
        Right("Recovered from " + str)
    }
    println(s"t2: $t2")
    // res13: Either[String,String] = Right(Recovered from error)

    "foo".asLeft[Int].leftMap(_.reverse)
    // res14: Either[String,Int] = Left(oof)

    6.asRight[String].bimap(_.reverse, _ * 7)
    // res15: Either[String,Int] = Right(42)

    "bar".asLeft[Int].bimap(_.reverse, _ * 7)
    // res16: Either[String,Int] = Left(rab)

    123.asRight[String]
    // res17: Either[String,Int] = Right(123)

    123.asRight[String].swap
    // res18: scala.util.Either[Int,String] = Left(123)


    /**
     * Fail-Fast Error Handling
     *  Either is typically used to implement fail-fast error handling.
     */
    for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if(b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String
        ]
    } yield c * 100
    // res19: scala.util.Either[String,Int] = Left(DIV0)

    /**
     * 4.4.5 Representing Errors
     */
    sealed trait LoginError extends Product with Serializable
    final case class UserNotFound(
      username: String
    ) extends LoginError

    final case class PasswordIncorrect(
      username: String
    ) extends LoginError

    case object UnexpectedError extends LoginError
    case class  User(username: String, password: String)

    type LoginResult = Either[LoginError, User]

    // Choose error-handling behaviour based on type:
    def handleError(error: LoginError): Unit =
      error match {
        case UserNotFound(u) =>
          println(s"User not found: $u")
        case PasswordIncorrect(u) =>
          println(s"Password incorrect: $u")
        case UnexpectedError =>
          println(s"Unexpected error")
      }
    val result1: LoginResult = User("dave", "passw0rd").asRight
    // result1: LoginResult = Right(User(dave,passw0rd))

    val result2: LoginResult = UserNotFound("dave").asLeft
    // result2: LoginResult = Left(UserNotFound(dave))

    result1.fold(handleError, println)
    // User(dave,passw0rd)

    result2.fold(handleError, println)
    // User not found: dave
  }

  def eitherTransform(): Unit ={
    import cats.syntax.either._

    /**
     * cats.syntax.either adds a number of useful methods to Either.
     * We can use orElse and getOrElse to extract values from the right side:
     *   the right value or return a default:
     */
    //def asLeft[B]: Either[A, B] = Left(obj)
    //    asLeft[RightType]
    println("Errrrror".asLeft[Int].getOrElse(0))
    // res9: Int = 0

    //def asRight[B]: Either[B, A] = Right(obj)
    //    asRight[LeftType]
    println("Errrrror".asRight[Int].getOrElse(0))
    //Errrrror

    println("Errrrror".asLeft[Int])
    //Left("Errrror") : Either[String, Int]

    println(2.asRight[String])
    //Right(2) : Either[String, Int]

    println("Errrrror".asLeft[Int].orElse(2.asRight[String]))
    //Right(2) : Either[String,Int]

    //The ensure method allows us to check whether a wrapped value satisfies a predicate:
    //  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): Either[AA, B]
    println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))
    // res11: Either[String,Int] = Left(Must be non-negative!)

    "error".asLeft[String] recover {
      case str: String =>
      "Recovered from " + str
    }
    // res12: Either[String,String] = Right(Recovered from error)

    "error".asLeft[String] recoverWith {
    case str: String =>
    Right("Recovered from " + str)
    }
    // res13: Either[String,String] = Right(Recovered from error)

    "foo".asLeft[Int].leftMap(_.reverse)
    // res14: Either[String,Int] = Left(oof)

    6.asRight[String].bimap(_.reverse, _ * 7)
    // res15: Either[String,Int] = Right(42)

    "bar".asLeft[Int].bimap(_.reverse, _ * 7)
    // res16: Either[String,Int] = Left(rab)

    123.asRight[String]
    // res17: Either[String,Int] = Right(123)

    123.asRight[String].swap
    // res18: scala.util.Either[Int,String] = Left(123)
  }

  def evalMonad(): Unit = {
    println("val ---- ")
    val x = {
      println("Computing X");
      1 + 1
    }
    // Computing X
    // x: Int = 2

    println("let's go!")
    println(x)  // first access
    // res0: Int = 2

    println(x) // second access
    // res1: Int = 2

    println("def ---- ")

    def y = {
      println("Computing Y")
      1 + 1
    }

    // y: Int

    println("let's go 2")
    println(y) // first access
    // Computing Y
    // res2: Int = 2

    println(y)  // second access
    // Computing Y
    // res3: Int = 2

    println("lazy ---- ")
    lazy val z = {
      println("Computing Z")
      1 + 1
    }
    // z: Int = <lazy>

    println("let's go 2")
    println(z)  // first access
    // Computing Z
    // res4: Int = 2
    println(z)  // second access
    // res5: Int = 2

    /**
     * Eval has three subtypes: Eval.Now, Eval.Later, and Eval.Always.
     */
    import cats.Eval
    // import cats.Eval
    val now = Eval.now(1 + 2)
    // now: cats.Eval[Int] = Now(3)

    val later = Eval.later(3 + 4)
    // later: cats.Eval[Int] = cats.Later@49373f5e

    val always = Eval.always(5 + 6)
    // always: cats.Eval[Int] = cats.Always@53dd8e4b

    println(now.value)
    // res6: Int = 3

    println(later.value)
    // res7: Int = 7

    println(always.value)
    // res8: Int = 11
  }

  def evalMonad2(): Unit = {
    import cats.Eval

    println("\nEval.now --- ")
    val x = Eval.now {
      println("Computing X")
      1+1
    }
    // Computing X

    println("first access")
    // x: cats.Eval[Int] = Now(2)
    x.value // first access
    // res9: Int = 2

    println("second access")
    x.value // second access
    // res10: Int = 2

    println("\nEval.always --- ")
    val y = Eval.always {
      println("Computing Y")
      1+1
    }
    // y: cats.Eval[Int] = cats.Always@253eef84

    println("first access")
    y.value // first access
    // Computing Y
    // res11: Int = 2

    println("second access")
    y.value // second access
    // Computing Y
    // res12: Int = 2

    println("\nEval.later --- ")
    val z = Eval.later {
      println("Computing Z")
      1+1
    }
    // z: cats.Eval[Int] = cats.Later@a81d4d5

    println("first access")
    z.value // first access
    // Computing Z
    // res13: Int = 2

    println("second access")
    z.value // second access
    // res14: Int = 2

    println("\ngreeting ---------")
    val greeting = Eval.always {
      println("Step 1")
      "Hello"
    }.map { str =>
      println("Step 2")
      str + " world"
    }
    // greeting: cats.Eval[String] = cats.Eval$$anon$8@3c88d726

    println("First Access")
    greeting.value
    // Step 1
    // Step 2
    // res15: String = Hello world
    println("Second Access")
    greeting.value
    println("Thrid Access")
    greeting.value


    println("\nfor comprehension ---- ")
    val ans = for {
      a <- Eval.now    { println("Calculating A") ; 40 }
      b <- Eval.always { println("Calculating B") ; 2  }
    } yield {
      println("Adding A and B")
      a+b
    }
    // Calculating A
    // ans: cats.Eval[Int] = cats.Eval$$anon$8@9284e40

    println("first access")
    ans.value // first access
    // Calculating B
    // Adding A and B
    // res16: Int = 42

    println("second access")
    ans.value // second access
    // Calculating B
    // Adding A and B
    // res17: Int = 42

    println("\nmemoize --------")
    val saying = Eval.always {
      println("Step 1")
      "The cat"
    }.map { str =>
      println("Step 2")
      s"$str sat on"
    }.memoize.map { str =>
      println("Step 3")
      s"$str the mat"
    }

    // saying: cats.Eval[String] = cats.Eval$$anon$8@397aee40
    saying.value // first access
    // Step 1
    // Step 2
    // Step 3
    // res18: String = The cat sat on the mat

    //Though you used Eval.always, Step 1 and 2 are memoized
    saying.value // second access
    // Step 3
    // res19: String = The cat sat on the mat
  }

  def stackOverFlow(): Unit ={
    def factorial(n: BigInt): BigInt =
      if(n == 1) n else n * factorial(n - 1)

    factorial(50000)
    // java.lang.StackOverflowError
    //   ...
  }

  def stillStackOverFlow(): Unit ={
    import cats.Eval

    def factorial(n: BigInt): Eval[BigInt] =
      if(n == 1) Eval.now(n) else factorial(n - 1).map(_ * n)

    factorial(50000).value
    // java.lang.StackOverflowError
    //   ...
  }

  def trampoline(): Unit ={
    import cats.Eval

    def factorial(n: BigInt): Eval[BigInt] =
      if(n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1).map(_ * n))
      }

    (factorial(50000).value)
  }

  def exercise1(): Unit = {
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      as match {
        case head :: tail =>
          fn(head, foldRight(tail, acc)(fn))
        case Nil =>
          acc
      }
    foldRight2((1 to 100000).toList, 0)(_ + _)

    import cats.Eval

    //See the difference in the signatures
    //  def foldRight[A, B](as: List[A], acc: B      )(fn: (A, B)       => B      ): B =
    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      //Inside the match expression, implementation is the same:
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil =>
          acc }

    //To make the signature in line
    //def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      foldRightEval(as, Eval.now(acc)) { (a, b) =>
        b.map(fn(a, _))
      }.value

    foldRight2((1 to 100000).toList, 0)(_ + _)
    // res22: Int = 705082704
  }

  def writerMonad(): Unit ={
    import cats.data.Writer
    import cats.instances.vector._

    println(
      Writer(Vector(
        "It was the best of times",
        "It was the worst of times"
      ), 123)
    )
    // res0: cats.data.WriterT[cats.Id,scala.collection.immutable.
    // Vector[String],Int] = WriterT((Vector(It was the best of times, It was the worst of times),123))

    /**
     * A Writer[W, A] carries two values: a log of type W and a result of type
     * In the spirit of code reuse, Cats implements Writer in terms of another type, WriterT.
     */
    //type Writer[W, A] = WriterT[Id, W, A]

    import cats.syntax.applicative._ // `pure` method
    type Logged[A] = Writer[Vector[String], A]

    println(123.pure[Logged])
    // res2: Logged[Int] = WriterT((Vector(),123))

    /**
     * If we have a log and no result, we can create a Writer[Unit] using the tell syntax
     */
    import cats.syntax.writer._
    Vector("msg1", "msg2", "msg3").tell
    // res3: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] = WriterT((Vector(msg1, msg2, msg3),()))
    //                                                                                        Unit, no result -> ^()

    import cats.syntax.writer._
    val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
    // a:    cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int] = WriterT((Vector(msg1, msg2, msg3),123))

    val b = 123.writer(Vector("msg1", "msg2", "msg3"))
    // b:    cats.data.Writer[scala.collection.immutable.Vector[String],Int] = WriterT((Vector(msg1, msg2, msg3),123))


    /**
     * We can extract the result and log from a Writer using the
     *   * `value` and
     *   * `written`
     * methods respectively:
     */
    a.value
    // res4: cats.Id[Int] = 123
    a.written
    // res5: cats.Id[scala.collection.immutable.Vector[String]] = Vector(msg1, msg2, msg3)

    /**
     * Or we can extract log and result at the same  me using the run method:
     */
    val (log, result) = b.run
    // log: scala.collection.immutable.Vector[String] = Vector(msg1,msg2, msg3)
    // result: Int = 123
  }

  def writerMonadTransform(): Unit ={
    import cats.data.{Writer}
    import cats.syntax.applicative._ // `pure` method
    import cats.syntax.writer._      // `writer` method
    import cats.instances.vector._

    type Logged[A] = Writer[Vector[String], A]

    //def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
    println(10.pure[Logged])
    //WriterT((Vector(),10))

    println(Vector("a", "b", "c").tell)
    //WriterT((Vector(a, b, c),())) <- tell has no result

    println(32.writer(Vector("x", "y", "z")))
    //WriterT((Vector(x, y, z),32))

    /**
     * flatMap actually appends the logs from the source Writer and the re- sult of the user’s sequencing func on.
     */
    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b
    // writer1: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((Vector(a, b, c, x, y, z),42))
    println("writer1")
    println(writer1.run)
    // res6: cats.Id[(Vector[String], Int)] = (Vector(a, b, c, x, y, z),42)

    /**
     * In addition to transforming the result with map and flatMap,
     * we can transform the log in a Writer with the mapWritten method:
     */
    val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
      // writer2: cats.data.WriterT[cats.Id,scala.collection.immutabl.Vector[String],Int]
      // = WriterT((Vector(A, B, C, X, Y, Z),42))

    println("writer2")
    println(writer2.run)
    // res7: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(A, B, C, X, Y, Z),42)

    /**
     * We can tranform both log and result simultaneously using bimap or mapBoth.
     */
    val writer3 = writer1.bimap(
      log    => log.map(_.toUpperCase),
      result => result * 100
    )
    // writer3: cats.data.WriterT[cats.Id,scala.collection.immutable
    //.Vector[String],Int] = WriterT((Vector(A, B, C, X, Y, Z),4200))

    println("writer3")
    println(writer3.run)
    // res8: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(A, B, C, X, Y, Z),4200)

    val writer4 = writer1.mapBoth { (log, result) =>
      val log2    = log.map(_ + "!")
      val result2 = result * 1000
      (log2, result2)
    }
    // writer4: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[String],Int]
    // = WriterT((Vector(a!, b!, c!, x!, y!, z!),42000))

    println("writer4")
    println(writer4.run)
    // res9: cats.Id[(scala.collection.immutable.Vector[String], Int)] = (Vector(a!, b!, c!, x!, y!, z!),42000)

    /**
     * Finally, we can clear the log with the reset method and swap log and result with the swap method:
     */
    val writer5 = writer1.reset
    // writer5: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((Vector(),42))

    println("writer5")
    println(writer5.run)
    // res10: cats.Id[(Vector[String], Int)] = (Vector(),42)
    val writer6 = writer1.swap
    // writer6: cats.data.WriterT[cats.Id,Int,Vector[String]] = WriterT((42,Vector(a, b, c, x, y, z)))

    println("writer6")
    println(writer6.run)
    // res11: cats.Id[(Int, Vector[String])] = (42,Vector(a, b, c, x, y, z))
  }

  def exerciseWriterMonad(): Unit ={
    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    factorial(5)
    // fact 0 1
    // fact 1 1
    // fact 2 2
    // fact 3 6
    // fact 4 24
    // fact 5 120
    // res13: Int = 120


    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    /**
     * Let's do this parallely!
     *  -> Interleaving log messages from two future executions
     */
    println()
    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)
    // fact 0 1
    // fact 0 1
    // fact 1 1
    // fact 1 1
    // fact 2 2
    // fact 2 2
    // fact 3 6
    // fact 3 6
    // res14: scala.collection.immutable.Vector[Int] =
    //   Vector(120, 120)

    /**
     * Solution
     */
    println()
    //We’ll start by defining a type alias for Writer so we can use it with pure syntax:
    import cats.data.Writer
    import cats.syntax.applicative._ // `pure` method
    import cats.instances.vector._

    type Logged[A] = Writer[Vector[String], A]
    42.pure[Logged]

    //We’ll import the tell syntax as well:
    import cats.syntax.writer._
    Vector("Message").tell
    // res16: cats.data.Writer[scala.collection.immutable.Vector[String],Unit] = WriterT((Vector(Message),()))

    //Finally, we’ll import the Semigroup instance for Vector. We need this to map and flatMap over Logged:
    import cats.instances.vector._
    41.pure[Logged].map(_ + 1)
    // res17: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((Vector(),42))

    def factorial2(n: Int): Logged[Int] =
      for {
        ans <- if(n == 0) {
          1.pure[Logged]
        } else {
          slowly(factorial2(n - 1).map(_ * n))
        }
        _   <- Vector(s"fact $n $ans").tell
      } yield ans

    val (log, result) = factorial2(5).run
    // log: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
    // result: Int = 120

    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(Future.sequence(Vector(
        Future(factorial2(5).run),
        Future(factorial2(5).run)
      )), 5.seconds)
    println(logA)
    // logA: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
    // ansA: Int = 120
    println(logB)
    // logB: Vector[String] = Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact 5 120)
    // ansB: Int = 120
  }

  def stateMonad(): Unit = {
    import cats.data.State
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }
    println(a)
    // a: cats.data.State[Int,String] = cats.data.StateT@ad944e3

    // Get the state and the result:
    val (state1, result1) = a.run(10).value
    // state: Int = 10
    // result: String = The state is 10

    // Get the state, ignore the result:
    val state2 = a.runS(10).value
    // state: Int = 10

    // Get the result, ignore the state:
    val result2 = a.runA(10).value

    println(state1)
    println(state2)
    println(result1)
    println(result2)

    /**
     * Composing and transforming state
     */
    val step1 = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }
    // step1: cats.data.State[Int,String] = cats.data.StateT@e680f25
    println(s"step1: ${step1}")

    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }
    println(s"step2: ${step2}")
    // step2: cats.data.State[Int,String] = cats.data.StateT@1119acde

    val both = for {
      a <- step1
      b <- step2
    } yield (b, a)

    /**
     * Hmmmm why the tuple becomes cats.data.StateT ... ?
     */
    println(s"both: ${both}")
    // both: cats.data.StateT[cats.Eval,Int,(String, String)] = cats.data.StateT@2d4d4ff3

    val (state3, result3) = both.run(20).value
    // state: Int = 42
    // result: (String, String) = (Result of step1: 21,Result of step2: 42)

    println(s"state3: ${state3}")
    println(s"result3: ${result3}")

    /**
     * Using flatMap & map intead of for comprehension.
     * Hmmmm why the tuple becomes cats.data.StateT ... ?
     */
    val bothequivalent = step1.flatMap {
      a => step2.map(b => (a, b))
    }
    println(s"bothequivalent: ${bothequivalent}")
    //bothequivalent: cats.data.StateT@31b0f1a8

    val (state4, result4) = bothequivalent.run(20).value
    println(s"state4: ${state4}")
    println(s"result4: ${result4}")
  }

  def stateProgram(): Unit =  {
    import cats.data.State

    val getDemo = State.get[Int]
    // getDemo: cats.data.State[Int,Int] = cats.data.StateT@26c929b1
    getDemo.run(10).value
    // res3: (Int, Int) = (10,10)
    val setDemo = State.set[Int](30)
    // setDemo: cats.data.State[Int,Unit] = cats.data.StateT@1748341a
    setDemo.run(10).value
    // res4: (Int, Unit) = (30,())
    val pureDemo = State.pure[Int, String]("Result")
    // pureDemo: cats.data.State[Int,String] = cats.data.StateT@1826901
    pureDemo.run(10).value
    // res5: (Int, String) = (10,Result)
    val inspectDemo = State.inspect[Int, String](_ + "!")
    // inspectDemo: cats.data.State[Int,String] = cats.data.StateT@77e7bb7e
      inspectDemo.run(10).value
    // res6: (Int, String) = (10,10!)

    val modifyDemo = State.modify[Int](_ + 1)
    // modifyDemo: cats.data.State[Int,Unit] = cats.data.StateT@56cf32b8
    modifyDemo.run(10).value
    // res7: (Int, Unit) = (11,())

    import State._
    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)
    // program: cats.data.State[Int,(Int, Int, Int)] = cats.data.StateT@4996bc50
    val (state, result) = program.run(1).value
    // state: Int = 3
    // result: (Int, Int, Int) = (1,2,3000)
    println(state)
    println(result)
  }

  def main(args: Array[String]): Unit = {
    Wrap("optionsExamples")(optionsExamples)
    Wrap("listMonads")(listMonads)
    Wrap("defaultInstances")(defaultInstances)
    Wrap("identityMonad")(identityMonad)
    Wrap("either")(either)
    Wrap("eitherTransform")(eitherTransform)
    Wrap("evalMonad")(evalMonad)
    Wrap("evalMonad2")(evalMonad2)
    //Wrap("stackOverFlow")(stackOverFlow)
    Wrap("trampoline")(trampoline)
    Wrap("exercise1")(exercise1)
    Wrap("writerMonad")(writerMonad)
    Wrap("writerMonadTransform")(writerMonadTransform)
    Wrap("exerciseWriterMonad")(exerciseWriterMonad)
    Wrap("stateMonad")(stateMonad)
    Wrap("stateProgram")(stateProgram)
  }
}
