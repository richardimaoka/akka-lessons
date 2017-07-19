package my.scalacheck

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary
import Gen._

object GeneratorSpec extends Properties("Int") {

  /**
   * Gen.choose()
   *
   * A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate
   *  any value.
   *
   *    def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen[T] =
   *      c.choose(min, max)
   */
  val myGen = for {
    n <- Gen.choose(10, 20)
    m <- Gen.choose(2 * n, 500)
  } yield (n, m)

  /**
   * Generate a sample value
   *   def sample: Option[T]
   */
  for(i <- 1 to 5)
    println(s"GeneratorSpec myGen: ${myGen.sample}")

  //property("something") = forAll { g <- myGen }

  /**
   * Picks a random value from a list:
   *   def oneOf[T](t0: T, t1: T, tn: T*): Gen[T] =
   *     oneOf(t0 +: t1 +: tn)
   */
  val vowel1 = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')

  for(i <- 1 to 10)
    println(s"GeneratorSpec vowel1: ${vowel1.sample}")


  /**
   *  Chooses one of the given generators with a weighted random distribution
   *    def frequency[T](gs: (Int, Gen[T])*): Gen[T]
   */
    val vowel2 = Gen.frequency(
    (3, 'A'),
    (4, 'E'),
    (2, 'I'),
    (3, 'O'),
    (1, 'U'),
    (1, 'Y')
  )

  for(i <- 1 to 10)
    println(s"GeneratorSpec vowel2: ${vowel2.sample}")


  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree

  val genLeaf = const(Leaf)

  val genNode = for {
    v <- arbitrary[Int]
    left <- genTree
    right <- genTree
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  for(i <- 1 to 10)
    println(s"GeneratorSpec genTree: ${genTree.sample}")

  /**
   * Conditional Generators
   *
   * Hmmm why does it result in Option[Int] !??
   */
  val smallEvenInteger = Gen.choose(0,200) suchThat (_ % 2 == 0)
  val smallEvenIntegerWithoutCondition = Gen.choose(0,200)

  for(i <- 1 to 10){
    println(s"GeneratorSpec smallEvenInteger                  : ${smallEvenInteger.sample}")
    println(s"GeneratorSpec smallEvenInteger without condition: ${smallEvenIntegerWithoutCondition.sample}")
  }


  /**
   * Generating Containers
   */
  val genIntList      = Gen.containerOf[List,Int](Gen.oneOf(1, 3, 5))

  val genStringStream = Gen.containerOf[Stream,String](Gen.alphaStr)

  val genBoolArray    = Gen.containerOf[Array,Boolean](true)

  for(i <- 1 to 10)
    println(s"GeneratorSpec genIntList: ${genIntList.sample}")

}

