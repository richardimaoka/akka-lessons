package my.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object CheckProps extends Properties("CheckProps") {

  val p1 = property("greater than") = forAll {
     i: Int =>
       (i > 0) ==> (i > i -1)
  }

}
