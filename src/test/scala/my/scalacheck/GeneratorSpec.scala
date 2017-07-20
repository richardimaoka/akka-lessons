package my.scalacheck

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary
import Gen._
import my.scalacheck.DebugGlobal.isDebugPrint

object GeneratorSpec extends Properties("Int") {

  /************************************************************************
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
  if(isDebugPrint)
    for(i <- 1 to 5)
      println(s"GeneratorSpec myGen: ${myGen.sample}")

  property("myGen") = forAll(myGen) { tuple =>
    if(isDebugPrint)
      for(i <- 1 to 5)
        println(s"GeneratorSpec myGen tuple = ${tuple}, class = ${tuple.getClass}, (${tuple._1}, ${tuple._2})")
    tuple._1 == tuple._2 || tuple._1 != tuple._2
  }

  /**************************************************************************
   * Gen.choose() another example
   *
   * Another property for small integers
   *
   */
  val smallInteger = Gen.choose(0,100)
  val propSmallInteger = Prop.forAll(smallInteger) { n =>
    {
      n >= 0
    }
  }
  /**
   * Interestingly this does not execute the property test (or at least invisible in test result)
   * with testOnly
   */
  propSmallInteger.check

  /************************************************************************
   *
   * Generator for Log
   *
   */
  property("for") = forAll( (long: Long) => {
    if(isDebugPrint)
      println(s"GeneratorSpec myGen long = ${long}")
    true
  }
  )

  /************************************************************************
   *
   * Generator for List
   *
   * You see te size of list is generally growing towards the end of
   * 100 examples
   *
   */
  property("forAllListInt") = forAll( (l: List[Int]) => {
    if(isDebugPrint)
      println(s"GeneratorSpec myGen l: List[Int] = ${l}")
    true
  }
  )
  /**************************************************************************
   * Picks a random value from a list:
   *   def oneOf[T](t0: T, t1: T, tn: T*): Gen[T] =
   *     oneOf(t0 +: t1 +: tn)
   */
  val vowel1 = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')

  if(isDebugPrint)
    for(i <- 1 to 20)
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

  if(isDebugPrint)
    for(i <- 1 to 30)
      println(s"GeneratorSpec vowel2: ${vowel2.sample}")


  /**************************************************************************
   *
   * Generator for your own type
   *
   */
  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree

  val genLeaf = const(Leaf)

  val genNode = for {
    v <- arbitrary[Int]
    left <- genTree
    right <- genTree
  } yield Node(left, right, v)

  //org.scalacheck.Gen
  // println(s"${genLeaf.getClass}")

  //org.scalacheck.Gen
  // println(s"${genNode.getClass}")

  def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  if(isDebugPrint)
    for(i <- 1 to 10)
      println(s"GeneratorSpec genTree: ${genTree.sample}")

  /**
   * Remember this syntax for your custom Gen
   */
  property("forAllTree") = forAll(genTree)( (t: Tree) => {
    if(isDebugPrint)
      println(s"GeneratorSpec forAllTree t: Tree = ${t}")
    true
  }
  )

  /**
   * You must also remember this syntax
   * When you want to create an `Arbitrary` which you expect the compiler to
   * implicitly resolve:
   *
   * (i.e.)      forAll { x => ... } syntax, instead of
   * insetead of forAll(gen){ x => ... }
   *
   * Then you have to implicitly define Arbitrary, by wrapping up a gen in Arbitrary()
   */
  implicit def arbTree: Arbitrary[Tree] = Arbitrary(oneOf(genLeaf, genNode))
  property("forAllTreeArbitrary") = forAll { (t: Tree) => {
    if (isDebugPrint)
      println(s"GeneratorSpec forAllTree t: Tree = ${t}")
    true
  }
  }

  /**************************************************************************
   * Conditional Generators
   *
   * Hmmm why does it result in Option[Int] !??
   */
  val smallEvenInteger = Gen.choose(0,200) suchThat (_ % 2 == 0)
  val smallEvenIntegerWithoutCondition = Gen.choose(0,200)

  if(isDebugPrint)
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

  if(isDebugPrint)
    for(i <- 1 to 10)
      println(s"GeneratorSpec genIntList: ${genIntList.sample}")

}

