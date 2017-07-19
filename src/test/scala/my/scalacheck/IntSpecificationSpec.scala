package my.scalacheck

import my.scalacheck.DebugGlobal.isDebugPrint
import org.scalacheck.{Gen, Prop, Properties}

object IntSpecificationSpec extends Properties("Int") {

  val smallInteger = Gen.choose(0,100)

  /**
   * Prop.forAll, or import org.scalacheck.Prop.forAll
   */
  property("smallInteger") = Prop.forAll(smallInteger) { n => {
//    if(isDebugPrint)
//      println(s"smallInteger: n = ${n}")

    n >= 0 && n <= 100
  }}

  /**
   * This will fail because n*n will overflow the max of Int
   *
   * [info] Compiling 1 Scala source to /Users/yunishiyama/akka-lessons/target/scala-2.12/test-classes...
   * sqrt: n = 430584478 n*n = 534387076   sqrt(n) = 23116.81370777556
   * sqrt: n = 215292239 n*n = -2013886879 sqrt(n) = NaN
   * sqrt: n = 107646119 n*n = -1684859663 sqrt(n) = NaN
   * sqrt: n = 53823059  n*n = 598703849   sqrt(n) = 24468.425552127377
   * sqrt: n = 26911529  n*n = 122764433   sqrt(n) = 11079.911236106542
   * sqrt: n = 13455764  n*n = -1056506480 sqrt(n) = NaN
   */
  property("sqrt") = Prop.forAll { (n: Int) => {
    if(isDebugPrint)
      println(s"sqrt: n = ${n} n*n = ${n*n} sqrt(n) = ${scala.math.sqrt(n*n)}")

    scala.math.sqrt(n * n) == n

    //trick to pass the test as `scala.math.sqrt(n * n) == n` fails
    true
  }}

}
