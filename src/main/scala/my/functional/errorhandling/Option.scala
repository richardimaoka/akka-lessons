package my.functional.errorhandling

import my.functional.errorhandling.Option.flagPrintRecurse
import my.wrapper.Wrap

import scala.util.Try
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => {
      if(flagPrintRecurse)
        println(s"map called on None, returning None")
      None
    }
    case Some(a) => {
      val ret = f(a)
      if(flagPrintRecurse)
        println(s"map called on ${this}, returning Some(${ret})")
      Some(ret)
    }
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => {
        if(flagPrintRecurse)
          println(s"flatMap called on None, returning None")
        None
      }
      case Some(a) => {
        val ret = f(a)
        if(flagPrintRecurse)
          println(s"flatMap called on ${this}, returning ${ret}")
        ret
      }
    }
  }
  /**
   * The below is probably better
   */
  //def flatMap[B](f: A => Option[B]): Option[B] =
  //  map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) if f(a) => this
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  private var flagPrintRecurse: Boolean = false
  private var flagPrintConstructor: Boolean = false

  def printRecurseCalls(f: => Unit): Unit = {
    flagPrintRecurse = true
    f
    flagPrintRecurse = false
  }

  def printConstractor(f: => Unit): Unit = {
    flagPrintConstructor = true
    f
    flagPrintConstructor = false
  }


  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = ???


  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](aOption: Option[A], bOption: Option[B])(f: (A, B) => C): Option[C] =
    /**
     * To avoid returning Option[Option], aOption's flatMap is used.
     * Then, flatMaps takes a function which accepts aOption's body, covert it to an Option.
     *
     * The body inside the resulting option is apparently f(aBody, bBody)
     */
    aOption flatMap (aBody => bOption map (bBody => f(aBody, bBody)))
  
  
  def map2_revised[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aBody <- a
      bBody <- b
    } yield f(aBody, bBody)
  /**
   * for comprehension can be either of below:
   *   if no return value, single loop
   *     -> collection.foreach
   *
   *   if no return value, nested loop
   *     -> collection.foreach(elem => genCollection(elem).foreach(subElem => elem ... subElem))
   *
   *   if return value, signle-loop
   *     -> collection.map
   *
   *   if return value, nested loop
   *     -> collection.flatmap(elem => genCollection(elem).map(subElem => elem ... subElem)
   *        //only the inner-most loop is map
   *
   * A mental image of map
   *    collection.flatMap where collection is like List(1,2,3,4,5,6,...)
   *
   * List(1,2,3,4,5,6,...) map f =  List(f(1),f(2),f(3),f(4),f(5),f(6),...)
   *   OK easy enough
   *
   * List(1,2,3,4,5,6,...) flatMap f
   *   //this is in two-steps
   *   step 1. the func f generates sub collections for each of the original element
   *   -> List(
   *       List( .... ),
   *       List( .... ),
   *       List( .... ),
   *       List( .... ),
   *      )
   *   step 2. concat the sub collections into the resulting (flattened) collection
   *     List( .................................. )
   */


  /*
  Here's an explicit recursive version:
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if(flagPrintRecurse)
      println(s"sequence called for a = ${a}")

    a match {
      case Nil => Some(Nil)
      case headElementInOption :: tailListOfOptions =>
        /**
         * Since we don't want to return Option, it should be flatMap, passing in a function: ... => Option to flatMap
         * For more details, explanation below
         */
        headElementInOption flatMap (headElementBody => sequence(tailListOfOptions) map (x => headElementBody :: x))
      /**
       * headElementInOption :: tailListOfOptions
       *   This is an important way to write down `data constructor` of Scala List.
       *   It is same as Cons(head,tail) as in the previous chapter
       *
       *
       * headElementInOption flatMap (headElementBody => sequence(tailListOfOptions) map (headElementBody :: _))
       *   uh, oh what is this!?
       *
       *   sequence(tailListOfOptions) : Option[List[A]]
       *     recursive call for tailList (i.e.) returns an Option,
       *     which holds a list with one less element
       *
       *   (headElementBody :: _) : List[A] => List[A]
       *     this is passed to Option's method `map`, so the underscore here `_` means an argument of List[A]
       *     this pre-pends headElementBody to `_` whose type is, again, List[A]
       *
       *   sequence(tailListOfOptions) map (headElementBody :: _) : Option[List[A]]
       *     From the above discussion,
       *       sequence(tailListOfOptions) returns Option[List[A]],
       *       Option[List[A]]'s map() method is called to pre-pend headElementBody to its internal List
       *
       *   headElementBody => sequence(tailListOfOptions) map (headElementBody :: _) : A => Option[List[A]]
       *     so, this is a function:
       *       passed into flatMap
       *       which takes the body (i.e. in A) of the head element (i.e. in Option[A]) and pre-pends body: A to the
       *       currently-and-internally-constructed Option[List[A]]'s withheld List[A]
       *       and return the Option[List[A]]
       *
       *     since Option's flatMap takes a function which returns an Option, ( T => Option[T] )
       *     the overall return value of sequence() is in Option[T] where T = List[A]
       *
       */
    }
  }

  /**
   * It can also be implemented using `foldRight` and `map2`.
   * The type annotation on `foldRight` is needed here; otherwise Scala wrongly infers the result type of the fold
   * as `Some[Nil.type]` and reports a type error (try it!).
   * This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
   *
   * listOfOptions = (e.g.) List( Some(1), Some(2), Some(3), ... )
   *
   * initial value = Some(Nil)
   *
   * use binary function (x,y) => map2(x,y)(_ :: _), append the currently constructed list
   * to be held inside Option
   */
  def sequence_1[A](listOfOptions: List[Option[A]]): Option[List[A]] =
    listOfOptions.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  def parseIntElement(str: String): Option[Int] =
    try{
      Some(str.toInt)
    }
    catch{
      case _: NumberFormatException => None
    }

  def traverseInEfficient[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(list.map(f))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    if(flagPrintRecurse)
      println(s"traverse called for ${a}")
    a match {
      case Nil => Some(Nil)
      case headElement :: tailList => map2(f(headElement), traverse(tailList)(f))(_ :: _)
    }
  }

  /**
   * Inefficient - listOfStrings map parseIntElement already went through the list already
   * before sequence() processes the returned list
   */
  def parseStringsToInts(listOfStrings: List[String]): Option[List[Int]] =
    sequence(listOfStrings map parseIntElement)


  def sequenceInTermsOfTraverse[A](listOfOptions: List[Option[A]]): Option[List[A]] =
    traverse(listOfOptions)(x => x)
    /**
     * Since this is listOfOptions, traverse[S,T]'s S is Option[A] and T is A
     *
     * sequenceInTermsOfTraverse[A](listOfOptions: List[Option[A]]): Option[List[A]] =
     *   traverse(listOfOptions)(x => x)
     *     // traverse[Option[A], A]
     *     // listOfOptions: List[Option[A]]
     *     // x => x: Option[A] => Option[A])
     */
}

object OptionTest {
  import Option._

  def testTypes(): Unit = {
    def check[A](list: List[Option[A]]): Unit =
      list match {
        case Nil => println("you gave me Nil. Sunil Swain.")
        case headElementInOption :: tailListOfOptions => {
          println(headElementInOption)
          println(tailListOfOptions)
        }
      }

    val listOfOptions: List[Option[Int]] = List(Some(1),Some(2),Some(3),None, Some(4))

    check(listOfOptions)
  }

  def flatMapTest(): Unit = {
    printRecurseCalls{
      Some(1).flatMap(x => Some((x + x + x).toString + " what the hell"))
      Some(1).map(x => Some((x + x + x).toString + " what the hell"))
    }
  }

  def testSequence(): Unit ={
    val listOfOptions: List[Option[Int]] = List(Some(1),Some(2),Some(3),None, Some(4), Some(5), Some(6))

    println(s"${listOfOptions} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequence(listOfOptions)
      println(a)
    }
    println()

    val listOfOptions2: List[Option[Int]] = List(Some(1),Some(2),Some(3),Some(4))

    println(s"${listOfOptions2} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequence(listOfOptions2)
      println(a)
    }
  }


  def testSequence_1(): Unit ={
    val listOfOptions: List[Option[Int]] = List(Some(1),Some(2),Some(3),None, Some(4), Some(5), Some(6))

    println(s"${listOfOptions} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequence_1(listOfOptions)
      println(a)
    }
    println()

    val listOfOptions2: List[Option[Int]] = List(Some(1),Some(2),Some(3),Some(4))

    println(s"${listOfOptions2} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequence_1(listOfOptions2)
      println(a)
    }
  }

  def testSequence_InTermsOfTraverse(): Unit ={
    val listOfOptions: List[Option[Int]] = List(Some(1),Some(2),Some(3),None, Some(4), Some(5), Some(6))

    println(s"${listOfOptions} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequenceInTermsOfTraverse(listOfOptions)
      println(a)
    }
    println()

    val listOfOptions2: List[Option[Int]] = List(Some(1),Some(2),Some(3),Some(4))

    println(s"${listOfOptions2} is converted to:" )
    printRecurseCalls{
      val a: Option[List[Int]] = sequenceInTermsOfTraverse(listOfOptions2)
      println(a)
    }
  }

  def parseIntTest(): Unit ={
    val list: List[String] = List("1", "2", "3", "4")

    println(s"${list} is converted to:" )
    println(parseStringsToInts(list))
    println()

    println(s"${list} is converted to:" )
    println(traverse(list)(parseIntElement))
    println()

    val invalidList: List[String] = List("1", "2", "bah", "3", "4")

    println(s"${invalidList} is converted to:" )
    println(parseStringsToInts(invalidList))
    println()

    println(s"${invalidList} is converted to:" )
    println(traverse(invalidList)(parseIntElement))
    println()
  }

  def main (args: Array[String]): Unit = {
    Wrap("test types")(testTypes)
    Wrap("flatMapTest")(flatMapTest)
    Wrap("test sequence")(testSequence)
    Wrap("test sequence_1")(testSequence_1)
    Wrap("testSequence_InTermsOfTraverse")(testSequence_InTermsOfTraverse)
    Wrap("parseIntTest")(parseIntTest)
  }
}