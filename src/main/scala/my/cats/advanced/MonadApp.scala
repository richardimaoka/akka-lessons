package my.cats.advanced

import my.wrapper.Wrap

object MonadApp {

  def optionsExamples(): Unit ={
    def parseInt(str: String): Option[Int] =
      scala.util.Try(str.toInt).toOption

    def divide(a: Int, b: Int): Option[Int] =
      if(b == 0) None else Some(a / b)

    println(s"""parseInt("1")  = ${parseInt("1")}""")
    println(s"""parseInt("2")  = ${parseInt("2")}""")
    println(s"""parseInt("-1") = ${parseInt("-1")}""")
    println(s"""parseInt("a")  = ${parseInt("a")}""")

    println(s"""divide(1, 2)  = ${divide(1, 2)}""")
    println(s"""divide(3, 2)  = ${divide(3, 2)}""")
    println(s"""divide(0, 2)  = ${divide(0, 2)}""")
    println(s"""divide(5, 0)  = ${divide(5, 0)}""")

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

  def main(args: Array[String]): Unit = {
    Wrap("optionsExamples")(optionsExamples)
  }
}
