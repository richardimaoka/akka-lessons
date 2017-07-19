package my.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, all, atLeastOne, BooleanOperators}

object CombineProps extends Properties("CombineProps") {

  val p1 = forAll { i: Int =>
    (i >= 0 && i < 10000) ==> (i + i == 2 * i)
  }

  val p2 = forAll{ i : Int =>
    (i >= 0 && i < 10000) ==> (i - i == 0)
  }

  val p3 = p1 && p2

  val p4 = p1 || p2

  val p5 = p1 == p2

  val p6 = all(p1, p2) // same as p1 && p2

  val p7 = atLeastOne(p1, p2) // same as p1 || p2

}
