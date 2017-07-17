package my.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object ListSpecificationSpec extends Properties("List") {
  /**
   * List.concat: OK, passed 100 tests.
   */
  property("concat") = forAll { (l1: List[Int], l2: List[Int]) => {
    //println(s"substring, l1: ${l1} l2: ${l2}")
    l1.size + l2.size == (l1 ::: l2).size
  }}
}
