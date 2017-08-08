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
     */
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

    import cats.instances.vector._
    Monad[Vector].flatMap(Vector(1, 2, 3))(x => Vector(x, x*10))
    // res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)
  }

  def main(args: Array[String]): Unit = {
    Wrap("optionsExamples")(optionsExamples)
    Wrap("listMonads")(listMonads)
  }
}
