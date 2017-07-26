package my.cats.advanced

import my.wrapper.Wrapper

object CatShow {

  def basics(): Unit = {
    import cats.Show

    //cats.instances.int is the Int instance of Show type class
    //cats.instances.int._ imports implicit methods
    import cats.instances.int._

    //cats.instances.string is the String instance of Show type class
    //cats.instances.string._ imports implicit methods
    import cats.instances.string._

    import cats.syntax.show._

    final case class Cat(name: String, age: Int, color: String)

    implicit val catShow = Show.show[Cat] { cat =>
      val name  = cat.name.show
      val age   = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }

    println(Cat("Garfield", 35, "ginger and black").show)
    // Garfield is a 35 year-old ginger and black cat.
  }

  def importDefaults(): Unit ={
    import cats.syntax.show._
    import cats.instances.int._

    // shownInt: String = 123
    val shownInt = 123.show
  }

  def customInstances(): Unit = {
    import cats.Show

    // Convert a function to a `Show` instance:
    def show[A](f: A => String): Show[A] = ???

    // Create a `Show` instance from a `toString` method:
    def fromToString[A]: Show[A] = ???

    import java.util.Date
    implicit val dateShow: Show[Date] =
      Show.show(date => s"${date.getTime}ms since the epoch.")
  }

  def main(args: Array[String]): Unit = {
    Wrapper("basics")(basics)
  }
}
