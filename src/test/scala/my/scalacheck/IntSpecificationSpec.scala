package my.scalacheck

import my.scalacheck.DebugGlobal.isDebugPrint
import org.scalacheck.{Gen, Prop, Properties}

object IntSpecificationSpec extends Properties("Int") {

  val smallInteger = Gen.choose(0,100)

  val propSmallInteger = Prop.forAll(smallInteger) { n =>
    n >= 0 && n <= 100
  }

  property("smallInteger") = Prop.forAll(smallInteger) { n => {
    if(isDebugPrint)
      println(n)

    n >= 0 && n <= 100
  }}

}
