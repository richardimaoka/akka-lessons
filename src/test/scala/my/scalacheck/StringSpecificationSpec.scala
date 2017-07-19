package my.scalacheck

import my.scalacheck.DebugGlobal.isDebugPrint
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringSpecificationSpec extends Properties("String") {

  /**
   * String.startsWith: OK, passed 100 tests.
   */
  property("startsWith") = forAll { (a: String, b: String) => {
    if(isDebugPrint)
      println(s"startsWith, a: ${a} b: ${b}")

    (a + b).startsWith(a)
  }}

  /**
   * String.endsWith: OK, passed 100 tests.
   */
  property("endsWith") = forAll { (a: String, b: String) =>
    (a+b).endsWith(b)
  }

  /**
   * String.concatenate: Falsified after 0 passed tests.
   * (due to two empty strings)
   * (a+b).length > a.length && (a+b).length > b.length
   *
   * So, adding thi condition will make the test pass
   */
  property("concatenate") = forAll { (a: String, b: String) => {
    if(isDebugPrint)
      println(s"concatenate, a: ${a} b: ${b}")

    (a+b).length > a.length && (a+b).length > b.length

    /**
     * ==> is imported by org.scalacheck.Prop.BooleanOperators
     */
    import org.scalacheck.Prop.BooleanOperators
    (a != "" && b != "") ==> {
      (a+b).length > a.length && (a+b).length > b.length
    }
  }}

  /**
   * String.substring: OK, passed 100 tests.
   */
  property("substring") = forAll { (a: String, b: String, c: String) => {
    if(isDebugPrint)
      println(s"substring, a: ${a} b: ${b}")

    (a+b+c).substring(a.length, a.length+b.length) == b
  }}
}
