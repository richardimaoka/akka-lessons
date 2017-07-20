package my.scalacheck

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop._
import DebugGlobal.isDebugPrint

/**
 * Collecting Generated Test Data
 *
 * Collect statistics about test data generated during property evaluation.
 * Useful to inspect the test case distribution.
 */
object GeneratedData extends Properties("Generated") {

  def ordered(l: List[Int]) = l == l.sorted

  /**
   * Now state the property, using Prop.classify to collect interesting information
   * on the generated data. The property itself is not very exciting in this example,
   * we just state that a double reverse should return the original list.
   */
  property("myProp") = forAll { l: List[Int] =>
    if (isDebugPrint)
      println(s"GeneratedData =: ${l}")

    /**
     * Collect data for presentation in test report
     * def classify(c: => Boolean, ifTrue: Any)(prop: Prop): Prop
     */
    classify(ordered(l), "ordered") {
      classify(l.length > 5, "large", "small") {
        if (isDebugPrint)
          println(s"GeneratedData =: ${l}")

        l.reverse.reverse == l
      }
    }
  }
  /**
   * Then you see something like below in the output:
   *
   * [info] > Collected test data:
   * [info] 75% large
   * [info] 14% small, ordered
   * [info] 11% small
   * [info] ScalaCheck
   * [info] Passed: Total 1, Failed 0, Errors 0, Passed 1
   *
   * Here ScalaCheck tells us that the property hasn't been tested
   * with any large and ordered list
   */


  /**
   * We can also collect data directly, using the Prop.collect method.
   */
  property("dummy") = forAll(Gen.choose(1,10)) { n =>
    collect(n) {
      n == n
    }
  }
  /**
   * Then you see something like below in the output:
   *
   * [info] > Collected test data: 
   * [info] 16% 6
   * [info] 14% 2
   * [info] 13% 1
   * [info] 11% 8
   * [info] 10% 7
   * [info] 10% 3
   * [info] 9% 9
   * [info] 8% 10
   * [info] 5% 5
   * [info] 4% 4
   */
}
