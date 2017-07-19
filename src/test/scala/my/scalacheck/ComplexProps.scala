package my.scalacheck

import org.scalacheck.Prop.{BooleanOperators, forAll, all}
import org.scalacheck.Properties

object ComplexProps extends Properties("ComplexProps") {

  def myMagicFunction(m: Int, n: Int): Int = {
    m * n
  }

  /**
   * Labeling Properties, with the :| operator
   *
   * ScalaCheck can tell you exactly what part is failing
   *
   * ! Falsified after 0 passed tests.
   * > Label of failing property: "result not sum"
   * > ARG_0: "0"
   * > ARG_1: "0"
   */
  val complexProp1 = forAll { (m: Int, n: Int) =>
    val res = myMagicFunction(n, m)
    (res >= m)    :| "result > #1" &&
      (res >= n)    :| "result > #2" &&
      (res < m + n) :| "result not sum"
  }

  /**
   * It is also possible to write the label before the conditions like this:
   *
   * result and label reversed in order
   *
   * Not the operator is |:, not :|
   */
  val complexProp2 = forAll { (m: Int, n: Int) =>
    val res = myMagicFunction(n, m)
    ("result > #1"    |: res >= m) &&
      ("result > #2"    |: res >= n) &&
      ("result not sum" |: res < m + n)
  }

  /**
   * The labeling operator can also be used to inspect intermediate values
   */
  val propMul = forAll { (n: Int, m: Int) =>
    val res = n*m
    ("evidence = " + res) |: all(
      "div1" |: m != 0 ==> (res / m == n),
      "div2" |: n != 0 ==> (res / n == m),
      "lt1"  |: res > m,
      "lt2"  |: res > n
    )
  }

}
