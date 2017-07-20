package my.scalacheck

import my.scalacheck.DebugGlobal.isDebugPrint
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object ArbitraryCustomSpec extends Properties("ArbitraryCustomSpec") {

  /**************************************************
   * For Int
   */
  property("usualInt") = forAll { (i: Int) =>
    if (isDebugPrint)
      println(s"usualInt: ${i}")
    true
  }

  {
    /**
     * Puts this in a scope so that the custom Int Arbitrary
     * does not affect the usualInt Arbitrary
     */
    implicit val arbCustomInt: Arbitrary[Int] = Arbitrary(Gen.oneOf(1,2,3,4,5,6))
    property("customInt") = forAll { (i: Int) =>
      if (isDebugPrint)
        println(s"customInt: ${i}")
      true
    }
  }

  /**************************************************
   * For String
   */
  property("usualString") = forAll { (s: String) =>
    if (isDebugPrint)
      println(s"usualString: ${s}")
    true
  }

  {
    /**
     * Puts this in a scope so that the custom Int Arbitrary
     * does not affect the usualInt Arbitrary
     */
    implicit val arbCustomString: Arbitrary[String] = Arbitrary(Gen.oneOf("abc", "bbb"))
    property("customString") = forAll { (s: String) =>
      if (isDebugPrint)
        println(s"customString: ${s}")
      true
    }
  }


}
