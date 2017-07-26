package my.cats.advanced

object SemiGroupApp {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  /**
   * There are four monoids for Boolean!
   */
  def exerciseBoolean(): Unit = {
    implicit val booleanAndMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = a && b
        def empty = true
      }

    implicit val booleanOrMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = a || b
        def empty = false
      }

    implicit val booleanEitherMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)
        def empty = false
      }

    implicit val booleanXnorMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean) = (!a || b) && (a || !b)
        def empty = true
      }
  }

  def exerciseSet(): Unit = {
    implicit def setUnionMonoid[A]: Monoid[Set[A]] =
      new Monoid[Set[A]] {
        def combine(a: Set[A], b: Set[A]) = a union b
        def empty = Set.empty[A]
      }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      def combine(a: Int, b: Int) = a + b
      def empty = 0
    }

    //Zup? no Monoid companion object we have defined so far
    //val intSetMonoid = Monoid[Set[Int]]

    //intSetMonoid.combine(Set(1, 2), Set(2, 3))
  }

  def main(): Unit = {

  }
}
