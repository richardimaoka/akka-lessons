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

  p1.check //+ OK, passed 100 tests.
  p2.check //+ OK, passed 100 tests.
  p3.check //! Gave up after only 23 passed tests. 501 tests were discarded.
  p4.check //+ OK, passed 100 tests.

  p5.check
  //! Falsified after 1 passed tests.
  //> ARG_0: 0
  //> ARG_1: -1090545228

  p6.check //! Gave up after only 26 passed tests. 501 tests were discarded.
  p7.check //+ OK, passed 100 tests.
}
