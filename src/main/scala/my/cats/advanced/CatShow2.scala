package my.cats.advanced

object CatShow2 {

  def bringTest(): Unit = {
    import cats.Show
    import cats.instances.int._

    //Accessing the `Show` type class instance for Int
    val showInt = Show.apply[Int]

    //invoking .show() method of the `Show` type class
    val intAsString: String = showInt.show(123)

  }

  def main(args: Array[String]): Unit = {
    import cats.Show

    /**
     * Instead of importing each type class instance
     * (e.g.) cats.instances.int._, cats.instances.double._, ....
     * Most people use this to bring all instances into scope at the same time.
     */
    import cats.instances.all._

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
}
