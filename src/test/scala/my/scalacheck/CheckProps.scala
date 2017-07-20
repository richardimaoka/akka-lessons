package my.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object CheckProps extends Properties("CheckProps") {

  val p1 = forAll {
     i: Int =>
       (i > 0) ==> (i > i -1)
  }

  println("p1.check started")

  /**
   * This will print out
   * + OK, passed 100 tests.
   */
  p1.check

  println("p1.check finished")

  /**
   * Conditional prop
   */
  var cnt = 0
  val p2 = forAll { i: Int =>
    cnt += 1
    println(s"conditional i = ${i}, (cnt = ${cnt})")
    (0 < i && i < 1000 ) ==> {
      true
    }
  }
  p2.check
  /**
   * This will print out like this:
   *
   * ...
   * ...
   * conditional i = 1, (cnt = 559)
   * conditional i = -2010345921, (cnt = 560)
   * conditional i = -1, (cnt = 561)
   * conditional i = 522872785, (cnt = 562)
   * ! Gave up after only 61 passed tests. 501 tests were discarded.
   *
   * See, there were 562 integers = 61 passed + 501 discarded
   *
   * "xxx tests were discarded" is printed out when you are using a conditional prop
   */
}
