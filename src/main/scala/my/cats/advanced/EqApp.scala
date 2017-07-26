package my.cats.advanced

object EqApp {

  def basics(): Unit ={
    import cats.Eq
    /**
     * This, not only includes .show() method, but also eq methods too
     * for the `Int` instance of the Eq type class.
     */
    import cats.instances.int._
    val eqInt = Eq[Int]

    eqInt.eqv(123, 123)
    // res1: Boolean = true

    eqInt.eqv(123, 234)
    // res2: Boolean = false

    //This import is needed to enable === and =!= methods below
    import cats.syntax.eq._

    123 === 123
    // res4: Boolean = true
    123 =!= 234
  }

  def catCompare(): Unit ={
    import cats.Eq
    import cats.syntax.eq._
    import cats.instances.option._ //this is also needed !

    final case class Cat(name: String, age: Int, color: String)

    val cat1 = Cat("Garfield",   35, "orange and black")
    val cat2 = Cat("Heathcliff", 30, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    /**
     * Another way to define this with Eq.instance[A] method is described later
     */
    implicit val catEqual = new Eq[Cat] {
      import cats.instances.int._
      import cats.instances.string._

      def eqv(cat1: Cat, cat2: Cat) =
        (cat1.name  === cat2.name ) &&
        (cat1.age   === cat2.age  ) &&
        (cat1.color === cat2.color)
    }

    /**
     * See here, you didn't create a type class instance of Eq[Option[Cat]]
     * but only created Eq[Cat]
     */
    optionCat1 === optionCat2
  }

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
