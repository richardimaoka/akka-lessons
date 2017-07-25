package my.cats.herding.day1

object ShowApp {

  import cats._
  import cats.data._
  import cats.implicits._

  def main(args: Array[String]): Unit ={
    /**
     * @typeclass trait Show[T] {
     *   def show(f: T): String
     * }
     */
    val a = 3.show // from cats.implicits
    println(s"${a}, ${a.getClass()}")

    val b = "hello".show
    println(s"${b}, ${b.getClass()}")

    val c = (new {}).toString
    // e.g. $anon$1@61a19b19
    println(c)

    /**
     * show is different from toString
     *
     * ShowApp.scala:25: value show is not a member of AnyRef
     * [error]     val d = (new {}).show
     * [error]                      ^
     * [error] one error found
     */
    //val d = (new {}).show


    /**
     * object Show provides two functions to create a Show instance:

        object Show {
          /** creates an instance of [[Show]] using the provided function */
          def show[A](f: A => String): Show[A] = new Show[A] {
            def show(a: A): String = f(a)
          }

          /** creates an instance of [[Show]] using object toString */
          def fromToString[A]: Show[A] = new Show[A] {
            def show(a: A): String = a.toString
          }

          implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
            def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
              show[B](fa.show _ compose f)
          }
        }
     *
     */

    case class Person(name: String, address: String)
    case class Car(model: String)

    implicit val personShow = Show.show[Person](_.name)
    implicit val carShow = Show.fromToString[Car]

    //Alice
    println(Person("Alice", "Toshima").show)

    //Person("Alice", "Toshima")
    println(Person("Alice", "Toshima"))

    //Car(CR-V)
    println(Car("CR-V"))

    //Car(CR-V)
    println(Car("CR-V"))
  }
}

