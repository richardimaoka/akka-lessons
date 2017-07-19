package my.scalacheck

import org.scalacheck.Properties

object MyAppSpecification extends Properties("MyApp") {
  include(CheckProps)
  include(CombineProps)
  include(ComplexProps)
  include(GeneratorSpec)
  include(IntSpecificationSpec)
  include(ListSpecificationSpec)
  include(StringSpecificationSpec)
}
