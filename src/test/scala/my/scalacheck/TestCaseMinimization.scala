package my.scalacheck

import org.scalacheck.{Properties, Shrink}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.Shrink._

object TestCaseMinimization extends Properties("TestCaseMinimization") {

  /**
   * No shrink
   */
  val p1 = forAllNoShrink(arbitrary[List[Int]])(l => l == l.distinct)

  /**
   * With shrink
   */
  val p2 = forAll(arbitrary[List[Int]])(l => l == l.distinct)
  val p3 = forAll( (l: List[Int]) => l == l.distinct )

  p1.check
  p2.check
  p3.check
  /**
   * ! Falsified after 7 passed tests.
   *   > ARG_0: List("2147483647", "2147483647", "-208607136", "-517154332", "1014957565")
   *
   * ! Falsified after 10 passed tests.
   *   > ARG_0: List("2147483647", "2147483647") //<- the list was shrunk into a list with just two identical elements in it
   *   > ARG_0_ORIGINAL: List("67956865", "2147483647", "1683984486", "872194427", "1874165740", "2147483647", "-971798265")
   *
   * ! Falsified after 11 passed tests.
   *   > ARG_0: List("1", "1")                   //<- the list was shrunk into a list with just two identical elements in it
   *   > ARG_0_ORIGINAL: List("1785394225", "-469233741", "1", "-1", "1", "-1984134618", "2147483647", "774346485")
   *
   * It's much easier to find a bug if you are given a simple test case that causes the failure.
   */



  /** Shrink instance of 2-tuple */
  implicit def shrinkTuple2[T1,T2](implicit s1: Shrink[T1], s2: Shrink[T2]
                                  ): Shrink[(T1,T2)] = Shrink {
    case (t1,t2) =>
      (for(x1 <- shrink(t1)) yield (x1, t2)) append
        (for(x2 <- shrink(t2)) yield (t1, x2))
  }
}
