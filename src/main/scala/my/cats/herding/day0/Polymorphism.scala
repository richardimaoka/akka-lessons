package my.cats.herding.day0

import my.wrapper.Wrap

object Polymorphism {

  def parametricPolymorphismTest(): Unit ={
    def head[A](xs: List[A]): A = xs(0)

    // 1
    println(head(1 :: 2 :: Nil))

    case class Car(make: String)

    // Car(Civic)
    println(head(Car("Civic") :: Car("CR-V") :: Nil))
  }

  def subtypePolymorphismTest(): Unit ={
    def plus[A](a1: A, a2: A): A = ???

    /**
     * plus takes only one parameters, hence dependency on `this`
     */
    trait PlusIntf[A] {
      def plus(a2: A): A
    }

    class IntPlus(a1: Int) extends PlusIntf[Int] {
      def plus(a2: Int): Int = this.a1 + a2
    }

    def plusBySubtype[A <: PlusIntf[A]](a1: A, a2: A): A = a1.plus(a2)

    val a1: IntPlus = new IntPlus(10)
    val a2: IntPlus = new IntPlus(100)
    //println( plusBySubtype(a1, a2))
  }

  def adhocPolymorphismTest(): Unit = {
    /**
     * plus takes two parameters, hence NO dependency on `this`
     */
    trait CanPlus[A] {
      def plus(a1: A, a2: A): A
    }
  }

  def main(args: Array[String]): Unit = {
    Wrap("parametricPolymorphismTest")(parametricPolymorphismTest)
    Wrap("subtypePolymorphismTest")(subtypePolymorphismTest)
  }
}
