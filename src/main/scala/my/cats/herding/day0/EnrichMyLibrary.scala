package my.cats.herding.day0


object EnrichMyLibrary {



  def testCase(): Unit ={
    //def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)

    import simulacrum._

    /**
     * See the difference from trait until the prev example
     * @typeclass annotation, as well as
     * @op annotation
     */
//    @typeclass trait Monoid[A] {
//      @op("|+|") def mappend(a: A, b: A): A
//      def mzero: A
//    }

//    object Monoid {
//      // "ops" gets generated
//      //val syntax = ops //compile error, somehow not generated
//
//      implicit val intMonoid = new Monoid[Int] {
//        def mappend(a: Int, b: Int): Int = a + b
//        def mzero: Int = 0
//      }
//
//      implicit val stringMonoid = new Monoid[String] {
//        def mappend(a: String, b: String): String = a + b
//        def mzero: String = ""
//      }
//    }

    //import Monoid.syntax._
    //println(3 |+| 4)
  }

  def catsTest(): Unit = {
    import cats._, cats.data._, cats.implicits._
    1.some
    1.some.orEmpty
  }

  def main(args: Array[String]): Unit ={

  }
}
