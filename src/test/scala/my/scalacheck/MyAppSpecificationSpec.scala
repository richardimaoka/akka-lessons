package my.scalacheck

import org.scalacheck.Properties

object MyAppSpecificationSpec extends Properties("MyApp") {
  include(ArbitrarySpec)
  include(CheckProps)
  include(CombineProps)
  include(ComplexProps)
  include(GeneratorSpec)
  include(IntSpecificationSpec)
  include(ListSpecificationSpec)
  include(StringSpecificationSpec)
}
