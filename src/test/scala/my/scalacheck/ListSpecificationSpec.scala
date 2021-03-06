package my.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import DebugGlobal._

object ListSpecificationSpec extends Properties("List") {
  /**
    * List.concat: OK, passed 100 tests.
    */
  property("concat") = forAll { (l1: List[Int], l2: List[Int]) => {
    if(isDebugPrint)
      println(s"list, l1: ${l1} l2: ${l2}")

    l1.size + l2.size == (l1 ::: l2).size
  }}

  property("reverse") = forAll { l: List[String] => {
    if(isDebugPrint)
      println(s"list, l: ${l}")

    l.reverse.reverse == l
  }}

  property("makeList") = forAll {
    n: Int => {
      import org.scalacheck.Prop.BooleanOperators

      if(isDebugPrint)
        println(s"makeList: n = ${n}")

      // ===> operator is imported by org.scalacheck.Prop.BooleanOperators
      (n >= 0 && n < 10000) ==> (List.fill(n)("").length == n)
    }
  }
}
