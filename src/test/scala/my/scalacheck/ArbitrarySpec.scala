package my.scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties, Prop}
import Gen.oneOf
import Prop.forAll

/**
 * There is a special generator, org.scalacheck.Arbitrary.arbitrary,
 * which generates arbitrary values of any supported type.
 */
object ArbitrarySpec extends Properties("ArbitrarySpec") {

  val evenInteger = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 0)

  val squares = for {
    xs <- Arbitrary.arbitrary[List[Int]]
  } yield xs.map(x => x*x)

  /**
   * Most of the times, you have to supply the type of the value to arbitrary
   */
  implicit lazy val arbBool: Arbitrary[Boolean] = Arbitrary(oneOf(true, false))


  /**
   *
   *
   * To get support for your own type T ...
   *
   *
   * you need to define an implicit def or val of type Arbitrary[T].
   * Use the factory method Arbitrary(...) to create the Arbitrary instance.
   */
  abstract sealed class Tree[T] {
    def merge(t: Tree[T]) = Internal(List(this, t))

    def size: Int = this match {
      case Leaf(_) => 1
      case Internal(children) => (children :\ 0) (_.size + _)
    }
  }
  case class Internal[T](children: Seq[Tree[T]]) extends Tree[T]
  case class Leaf[T](elem: T) extends Tree[T]

  /**
   * ... you need to define an implicit def or val of type Arbitrary[T].
   * Use the factory method Arbitrary(...) to create the Arbitrary instance.
   */

  implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] =
    Arbitrary {
      /**
       * Arbitrary of [T] (i.e. not [Tree[T]])
       */
      val genLeaf = for(e <- Arbitrary.arbitrary[T]) yield Leaf(e)

      def genInternal(sz: Int): Gen[Tree[T]] = for {
        n <- Gen.choose(sz/3, sz/2)
        c <- Gen.listOfN(n, sizedTree(sz/2))
      } yield Internal(c)

      def sizedTree(sz: Int) =
        if(sz <= 0) genLeaf
        /**
         * Chooses one of the given generators with a weighted random distribution
         *   def frequency[T](gs: (Int, Gen[T])*): Gen[T]
         */
        else Gen.frequency((1, genLeaf), (3, genInternal(sz)))


      /**
       * Creates a generator that can access its generation size
       *   def sized[T](f: Int => Gen[T]): Gen[T]
       */
      Gen.sized(sz => sizedTree(sz))
    }

  /**
   * As long as the implicit arbTree function is in scope, you can now write properties like this:
   */
  val propMergeTree = forAll { (t1: Tree[Int], t2: Tree[Int]) =>
    t1.size + t2.size == t1.merge(t2).size
  }
}
