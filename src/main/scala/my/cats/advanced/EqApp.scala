package my.cats.advanced

object EqApp {

  def main(args: Array[String]): Unit = {
    import cats.Eq
    import cats.syntax.eq._

    final case class Cat(name: String, age: Int, color: String)

    implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
      import cats.instances.int._
      import cats.instances.string._
      (cat1.name  === cat2.name ) &&
        (cat1.age   === cat2.age  ) &&
        (cat1.color === cat2.color)
    }

    val cat1 = Cat("Garfield",   35, "orange and black")
    // cat1: Cat = Cat(Garfield,35,orange and black)
    println(cat1)

    val cat2 = Cat("Heathcliff", 30, "orange and black")
    // cat2: Cat = Cat(Heathcliff,30,orange and black)
    println(cat2)

    cat1 === cat2
    // res14: Boolean = false
    println(cat1 === cat2)

    cat1 =!= cat2
    println(cat1 =!= cat2)
    // res15: Boolean = true

    import cats.instances.option._
    val optionCat1 = Option(cat1)
    // optionCat1: Option[Cat] = Some(Cat(Garfield,35,orange and black))
    println(optionCat1)

    val optionCat2 = Option.empty[Cat]
    // optionCat2: Option[Cat] = None
    println(optionCat2)

    optionCat1 === optionCat2
    // res16: Boolean = false
    println(optionCat1 === optionCat2)

    optionCat1 =!= optionCat2
    // res17: Boolean = true
    println(optionCat1 =!= optionCat2)
  }

}
