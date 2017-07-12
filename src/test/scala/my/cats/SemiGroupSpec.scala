package my.cats

import cats.kernel.Semigroup
import org.scalatest.{Matchers, WordSpec}

class SemiGroupSpec extends WordSpec with Matchers {
  import cats.implicits._


  "satisfy these " in {
    Semigroup[Int].combine(1, 2) should be( 3  )

    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(
      List(1,2,3,4,5,6)
    )

    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(
      Some(3)
    )

    Semigroup[Option[Int]].combine(Option(1), None) should be(
      Some(1)
    )

    /**
     * Hmm don't understand this...
     */
    Semigroup[Int ⇒ Int]
      .combine({ (x: Int) ⇒
        x + 1
      }, { (x: Int) ⇒
        x * 10
      })
      .apply(6) should be(
      67 // (x => x + 1)(6) combine (x => x * 10)(6)
    )

  }
}
