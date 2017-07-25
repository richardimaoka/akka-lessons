package my.cats.advanced

object CatShow {

  def main(args: Array[String]): Unit = {
    import cats.Show
    import cats.instances.int._
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
}
