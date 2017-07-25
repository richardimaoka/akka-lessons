package my.cats.advanced

import my.wrapper.Wrapper

object TypeClassBasics {

  def typeClass1(): Unit ={
    import JsonWriterInstances._

    /**
     * def toJson[A](value: A)(implicit w: JsonWriter[A]): Json
     *
     * JsonWriter instance is passed implicitly due to the above import
     */
    Json.toJson(Person("Dave", "dave@example.com"))
    // res4: Json = JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))

  }

  def typeClass2(): Unit ={
    import JsonWriterInstances._
    import JsonSyntax._

    /**
     * def toJson(implicit w: JsonWriter[A]): Json
     *
     * both import JsonWriterInstances (which includes implicit parameters to pass to `toJson`)
     * and  import JsonSyntax (`toJson` definition)
     *
     * is needed.
     */
    Person("Dave", "dave@example.com").toJson
    /// res5: Json = JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))
  }

  /*************************************************
   *  Exercises
   *************************************************/

  /**
   * These steps define the three main components of our type class.
   * First we define Printable—the type class itself:
   */
  trait Printable[A] {
    def format(value: A): String
  }

  /**
   * Then we define some ***default*** instances of
   * Printable and package then in PrintableInstances:
   */
  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      def format(input: String) = input
    }
    implicit val intPrintable = new Printable[Int] {
      def format(input: Int) = input.toString
    }
  }

  def exercise0(): Unit = {
    import PrintableInstances._ //this is needed!!!

    def printWithFormat[A](a: A)(implicit printable: Printable[A]): Unit = {
      println(printable.format(a))
    }

    printWithFormat(1)
  }

  def exercise1(): Unit = {
    import PrintableInstances._ //this is needed!!!

    /**
     * Finally we define an interface object, Printable:
     */
    object Printable {
      def format[A](input: A)(implicit p: Printable[A]): String =
        p.format(input)
      def print[A](input: A)(implicit p: Printable[A]): Unit =
        println(format(input))
    }

    Printable.format(1)
    Printable.print(1)
  }

  def exercise2(): Unit = {
    object Printable {
      implicit val stringPrintable = new Printable[String] {
        def format(input: String) = input
      }
      implicit val intPrintable = new Printable[Int] {
        def format(input: Int) = input.toString
      }
      def print[A](a: A)(implicit printable: Printable[A]): Unit = {
        println(printable.format(a))
      }
      def format[A](input: A)(implicit p: Printable[A]): String =
        p.format(input)
    }

    final case class Cat(
      name: String,
      age: Int,
      color: String
    )
    /**
     * Then we define type class instances for the types we care about.
     * These either go into the companion object of Cat or a separate object
     * to act as a namespace:
     */
    object Cat {
      import Printable._
      implicit val catPrintable = new Printable[Cat] {
        def format(cat: Cat): String  = {
          val name = Printable.format(cat.name)
          val age = Printable.format(cat.age)
          val color = Printable.format(cat.color)
          s"$name is a $age year-old $color cat."
        }
      }
    }

    val cat = Cat("nyatasha", 5, "blue")

    Printable.print(cat)
  }

  def exercise3(): Unit = {
    trait Printable[A] {
      def format(value: A): String
    }

    object Printable {
      implicit val stringPrintable = new Printable[String] {
        def format(input: String) = input
      }
      implicit val intPrintable = new Printable[Int] {
        def format(input: Int) = input.toString
      }
      def print[A](a: A)(implicit printable: Printable[A]): Unit = {
        println(printable.format(a))
      }
      def format[A](input: A)(implicit p: Printable[A]): String =
        p.format(input)
    }

    final case class Cat(
                          name: String,
                          age: Int,
                          color: String
                        )
    object Cat {
      import Printable._
      implicit val catPrintable = new Printable[Cat] {
        def format(cat: Cat): String  = {
          val name = Printable.format(cat.name)
          val age = Printable.format(cat.age)
          val color = Printable.format(cat.color)
          s"$name is a $age year-old $color cat."
        }
      }
    }

    /**
     * First we define an implicit **class** containing our extension methods:
     */
    object PrintableSyntax {
      implicit class PrintOps[A](value: A) {
        def format(implicit p: Printable[A]): String =
          p.format(value)
        def print(implicit p: Printable[A]): Unit =
          println(p.format(value))
      }
    }

    /**
     * See the .print method - enrich my library pattern ??
     */
    import PrintableSyntax._
    Cat("Garfield", 35, "ginger and black").print
    // Garfield is a 35 year-old ginger and black cat.
  }

  def main(args: Array[String]): Unit = {
    Wrapper("exercise1")(exercise1)
    Wrapper("exercise2")(exercise2)
    Wrapper("exercise3")(exercise3)
  }
}
