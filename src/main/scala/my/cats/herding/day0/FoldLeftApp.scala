package my.cats.herding.day0

import my.wrapper.Wrap

object FoldLeftApp {

  trait Monoid[A] {
    def mappend(a: A, b: A): A
    def mzero: A
  }

  object Monoid {
    implicit val intMonoid = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    implicit val stringMonoid = new Monoid[String] {
      def mappend(a: String, b: String): String = a + b
      def mzero: String = ""
    }
  }

  def firstCaseTest(): Unit = {

    object FoldLeftList {
      //def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)

      def foldLeft[A, B](xs: List[A])(b: B)(f: (B, A) => B) = xs.foldLeft(b)(f)
    }

    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      //FoldLeftList.foldLeft(xs, m.mzero, m.mappend)
      FoldLeftList.foldLeft(xs)(m.mzero)(m.mappend)
    }

    println( sum(List(1, 2, 3, 4)) )

    println( sum(List("a", "b", "c")) )

  }

  def secondAttemptTest(): Unit = {

    trait FoldLeft[F[_]] {
      def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
    }

    object FoldLeft {
      implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
        def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
      }
    }

    def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
      val m = implicitly[Monoid[A]]
      val fl = implicitly[FoldLeft[M]]
      fl.foldLeft(xs, m.mzero, m.mappend)
    }
  }

  def main(args: Array[String]): Unit = {
    Wrap("firstCaseTest")(firstCaseTest)
  }
}
