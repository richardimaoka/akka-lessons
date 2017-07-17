package my.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringSpecificationSpec extends Properties("String") {

  /**
   * String.startsWith: OK, passed 100 tests.
   */
  property("startsWith") = forAll { (a: String, b: String) => {
    //println(s"startsWith, a: ${a} b: ${b}")
    (a + b).startsWith(a)
  }}


  /**
   * String.concatenate: Falsified after 0 passed tests.
   * (due to two empty strings)
   */
  property("concatenate") = forAll { (a: String, b: String) => {
    //println(s"concatenate, a: ${a} b: ${b}")
    (a+b).length > a.length && (a+b).length > b.length
  }}

  /**
   * String.substring: OK, passed 100 tests.
   */
  property("substring") = forAll { (a: String, b: String, c: String) => {
    //println(s"substring, a: ${a} b: ${b}")
    (a+b+c).substring(a.length, a.length+b.length) == b
  }}
}
