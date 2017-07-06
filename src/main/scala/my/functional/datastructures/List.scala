package my.functional.datastructures

import my.functional.datastructures.List.sum
import my.wrapper.{Wrap, Wrapper}

sealed trait List[+A] {
  def flagPrintConstructor: Boolean = List.flagPrintConstructor
}// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] { // A `List` data constructor representing the empty list
  override def toString: String = "Nil"
}
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  if(flagPrintConstructor) println(s"constructor called for head = ${head} and tail= ${tail}")

  override def toString: String =
    "List(" + List.foldRightHidden(this, ")")( (a, str) => a.toString + "," + str )
}

object List { // `List` companion object. Contains functions for creating and working with lists.

  private var flagPrintFold: Boolean = false
  private var flagPrintConstructor: Boolean = false

  def printFold(f: => Unit): Unit = {
    flagPrintFold = true
    f
    flagPrintFold = false
  }

  def printConstractor(f: => Unit): Unit = {
    flagPrintConstructor = true
    f
    flagPrintConstructor = false
  }

  def apply[A](as: A*): List[A] = {
    //println("apply called for variadic args")

    // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => {
        //println("append() executes internal append()")
        Cons(h, append(t, a2))
      }
    }

  def foldRightHidden[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightHidden(xs, z)(f))
    }
  }

  /**
   * Our implementation of foldRight is not tail-recursive and will result in a StackOver- flowError
   * for large lists (we say it’s not stack-safe).
   *
   * Compiler error:
   * > could not optimize @tailrec annotated method foldRight: it contains a recursive call not in tail position
   */
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    if(flagPrintFold) println(s"foldRight called for l = ${l} and z = ${z}")
    // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
   * Unlike foldRight, this should be tail-recursive
   */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    if(flagPrintFold) println(s"foldLeft called for l = ${l} z = ${z}")

    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
    }
  }


  def length[A](l: List[A]): Int =
    foldRight(l, 0)( (_,x) => x + 1)

  /*
    Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
    a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
    returning a value just means this bug will be discovered later, further from the place where it was introduced.

    It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
    right hand side of a pattern. This makes it clear the value isn't relevant.
    */
  def tail[A](l: List[A]): List[A] = l match {
    //case Nil => Nil
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  /*
  If a function body consists solely of a match expression, we'll often put the match on the same line as the
  function signature, rather than introducing another level of nesting.
  */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    //case Nil => Nil
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  /*
  Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
  contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
  is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
  length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
  If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
  */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }


  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
  satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
  use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  /**
   * one with more friendly type inference
   * caller doesn't need to specify the argument type of f, unlike the above one
   */
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile2(t)(f)
    case _ => as }

  /*
  Note that we're copying the entire list up until the last element.
  Besides being inefficient, the natural recursive
  solution will use a stack frame for each element of the list, which can lead to stack overflows for
  large lists (can you see why?). With lists, it's common to use a temporary,
  mutable buffer internal to the
  function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this).
  So long as the buffer is allocated internal to the function,
  the mutation is not observable and RT is preserved.

  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
  doesn't require even local mutation. We'll write a reverse function later in this chapter.
  */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  //@annotation.tailrec
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /*
    The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
    when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
    when we discuss laziness).

    The other implementations build up a chain of functions which, when called, results in the operations being performed
    with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
    calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
    using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
    more of theoretical interest - they aren't stack-safe and won't work for large lists.
  */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation
  performed by `foldRight`.
  */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  /*
    Since `append` takes time proportional to its first argument, and this first argument never grows because of the
    right-associativity of `foldRight`, this function is linear in the total length of all lists. You may want to try
    tracing the execution of the implementation on paper to convince yourself that this works.

    In Scala,all methods whose names end in : are right-associative. That is,the expression x::xs is actually the method
    call xs.::(x), which in turn calls the data constructor ::(x,xs). See the Scala language specification for more information.

    Note that we're simply referencing the `append` function, without writing something like `(x,y) => append(x,y)`
    or `append(_,_)`. In Scala there is a rather arbitrary distinction between functions defined as _methods_, which are
    introduced with the `def` keyword, and function values, which are the first-class objects we can pass to other
    functions, put in collections, and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
    In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
    or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments aren't known.
  */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  /**
   * This is fold*Right* as it needs to apply +1 for each element.
   *
   * List() is of the form Cons(x, xs), so to re-construct the "+1"-applied Cons,
   * you need to call Cons(f(x), xs) (i.e.) at this point xs must only have
   * elements which applied +1 already
   */
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def doubleToString(l: List[Int]): List[String] =
    foldRight(l, Nil:List[String])((x,xs) => Cons(x.toString, xs))

  /*
    A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
    use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
    implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
    mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def map_1[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def map_3[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft_1(l, Nil:List[B])((h,t) => Cons(f(h),t))


  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((x, xs)=> if(f(x)) Cons(x,xs) else xs )
  }

  /*
    This could also be implemented directly using `foldRight`.
    */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) List(x) else Nil)


  /*
  To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
  syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
  objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
  right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
  variables introduced in the outer `match`.

  The discussion about stack usage from the explanation of `map` also applies here.
  */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  /*
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
}

object ListTest {
  import List._

  def listTest(): Unit = {
    def z(list: List[Int]): Int = list match {
      case Cons(x, Cons(2, Cons(4, _))) => {
        println("Hit the case: Cons(x, Cons(2, Cons(4, _))) ")
        x
      }
      case Nil => {
        println("Hit the case: Nil")
        42
      }
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => {
        println("Hit the case: Cons(x, Cons(y, Cons(3, Cons(4, _))))")
        x + y
      }
      case Cons(h, t) => {
        println("Hit the case: Cons(h, t) ")
        h + sum(t)
      }
      case _ => {
        println("Hit the case: _ ")
        101
      }
    }

    println(z(List(1,2,3,4,5)))
    println(z(List(1)))
    println(z(List(3,3,3,4,3,3)))
    println(z(List(3,3,3,5,3,3)))
    println(z(List(3,3,6,3,4,3,3)))
    println(z(List(1,2)))
    println(z(List()))
  }

  def tailTest(): Unit ={
    println(s"tail(List(1,2,3)) = ${tail(List(1,2,3))} in class = ${tail(List(1,2,3)).getClass}")
  }

  def setHeadTest(): Unit ={
    println(s"setHead(List(1,2,3), 10) = ${setHead(List(1,2,3), 10)}")
  }

  def dropTest(): Unit = {
    val a = List(1,2,3)
    /**
     * As in p. 35 of the text,
     *
     * When we call it with an anonymous function for f,
     * we have to specify the type of its argument, here named x:
     */
    dropWhile(a, (x: Int) => x > 2)

    /**
     * Scala can infer this fact if we group dropWhile into two argument lists
     *
     * That is, dropWhile(xs) is returning a function, which we then call with the argument f (in other words, dropWhile is curried).
     * The main reason for grouping the arguments this way is to assist with type inference.
     *
     * More generally, when a function definition contains multiple argument groups,
     * type information flows from left to right across these argument groups.
     * Here, the first argument group fixes the type parameter A of dropWhile to Int,
     * so the annotation on x => x < 4 is not required
     * We’ll often group and order our function arguments into multiple argument lists to maximize type inference.
     */
    dropWhile2(a)(x => x >2)
  }

  def appendTest(): Unit ={
    append(List(1,2,3), List(1,2,3))
  }

  def constructorTest(): Unit ={
    println("List(1)")
    val a = List(1)

    println("List(1,2,3)")
    val b = List(1,2,3)

    println("List(1, b)")
    val c = List(1, b)

    println("append(b, a)")
    val d = append(b, a)
  }

  def initTest(): Unit = {
    println(s"init(List(1,2,3,4))=${init(List(1,2,3,4))}")
  }

  def foldRightTest(): Unit = {
    /**
     *  This doesn't compile as `Nil` is missing its type and the type inference for Cons(_,_) cannot work
     *  [error] /Users/..../akka-lessons/src/main/scala/my/functional/datastructures/scala:251: type mismatch;
     *  [error]  found   : my.functional.datastructures.Cons[Int]
     *  [error]  required: my.functional.datastructures.Nil.type
     *  [error]     println( foldRight(List(1,2,3), Nil)(Cons(_,_)) )
     *                                                            ^
     */
    //println( foldRight(List(1,2,3), Nil)(Cons(_,_)) )

    /*
    We get back the original list! Why is that? As we mentioned earlier,
    one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor
    of the list with the `z` argument, and it replaces the `Cons` constructor with the given function,
    `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input 

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
    Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
    Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
    Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
    Cons(1, Cons(2, Cons(3, Nil)))

    To verify, this prints out:
      foldRight called
      foldRight called
      foldRight called
      foldRight called
      constructor called for head:A and tail: List[A]
      constructor called for head:A and tail: List[A]
      constructor called for head:A and tail: List[A]
    */
    print( foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_)))
  }

  def foldLeftTest(): Unit = {
    /**
     *   def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
     *     case Nil => z
     *     case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
     *   }
     *
     *   foldLeft(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])((xs,x) => Cons(x,xs))
     *   foldLeft(Cons(2, Cons(3, Nil)), Cons(1, Nil:List[Int]))((xs,x) => Cons(x,xs))
     *   foldLeft(Cons(3, Nil), Cons(2, Cons(1, Nil:List[Int])))((xs,x) => Cons(x,xs))
     *   foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil:List[Int])))((xs,x) => Cons(x,xs))
     *   Cons(3, Cons(2, Cons(1, Nil:List[Int])))
     *
     *   To verify, this prints out:
     *     foldLeft called
     *     foldLeft called
     *     foldLeft called
     *     foldLeft called
     *     List(3,2,1,)
     */
    print(foldLeft(List(1,2,3), Nil:List[Int])((xs/*accum*/,x/*next val*/) => Cons(x,xs)))
  }

  def lengthTest(): Unit ={
    println(length(List(1,2,3,4,5)))
  }

  def sumTest(): Unit ={
    sum(List(1,2,3,4,5,6,7,8,9,10))
    sum2(List(1,2,3,4,5,6,7,8,9,10))
  }
  
  def productTest(): Unit = {
    product(List(1,2,3,4,5,6,7,8,9,10))
    product2(List(1,2,3,4,5,6,7,8,9,10))
  }

  def foldVarious(): Unit = {
    println("foldRightViaFoldLeft")
    foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_,_))

    println("foldRightViaFoldLeft_1")
    foldRightViaFoldLeft_1(List(1, 2, 3), Nil: List[Int])(Cons(_,_))

    println("foldRightViaFoldLeft_2")
    foldLeftViaFoldRight(List(1, 2, 3), Nil: List[Int])((xs/*accum*/,x/*next val*/) => Cons(x,xs))
  }

  def toStringTest(): Unit ={
    println(List(1,2,3))
  }

  def appendTest2(): Unit ={
    val l_1 = List(1)
    val l2 = List(2)
    val l_1_2 = List(1,2)
    val l_1_2_3_4_5 = List(1,2,3,4,5)
    val l_10_20 = List(10, 20)
    val l_10_20_30_40_50 = List(10, 20, 30, 40, 50)


    printFold {
      printConstractor {
        val a = appendViaFoldRight(l_1, l2)
        println(a)
        val b = appendViaFoldRight(l_1_2, l_10_20)
        println(b)
        val c = appendViaFoldRight(l_1_2_3_4_5, l_10_20)
        println(c)
        val d = appendViaFoldRight(l_1_2_3_4_5, l_10_20_30_40_50)
        println(d)
      }
    }
  }

  def concatTest(): Unit ={
    val l_1_2_3_4_5 = List(1,2,3,4,5)
    val l_10_20_30_40_50 = List(10, 20, 30, 40, 50)
    val l_100_200 = List(100, 200)
    val l_1000_2000 = List(1000, 2000)

    val arg1 = List(l_1_2_3_4_5,l_10_20_30_40_50)
    val arg2 = List(l_1_2_3_4_5,l_10_20_30_40_50,l_100_200)
    val arg3 = List(l_1_2_3_4_5,l_10_20_30_40_50,l_100_200,l_1000_2000)

    printFold {
      printConstractor{
        val d = concat(arg1)
        println(d)
        val e = concat(arg2)
        println(e)
        val f = concat(arg3)
        println(f)
      }
    }
  }

  def mapTest(): Unit = {
    printFold {
      println(map(List(1,2,3))(_ + 10))
      println()
      println(map_1(List(1,2,3))(_ + 10))
      println()
      println(map_3(List(1,2,3))(_ + 10))
    }
  }

  def flatMapTest(): Unit = {
    val l = List(1,2,3)
    val f: Int => List[Int] = elem => List(elem, elem, elem)

    println(flatMap(l)(f))
  }

  def filterTest(): Unit = {
    val l = List(1,2,3)
    val f: Int => Boolean = x => x % 2 == 0

    printFold{
      println(filterByFlatMap(l)(f))
    }
  }

  def main(args: Array[String]): Unit = {
//    Wrapper("constructorTest")(constructorTest)
//    Wrapper("listTest")(listTest)
//    Wrapper("tailTest")(tailTest)
//    Wrapper("dropTest")(setHeadTest)
//    Wrapper("setHeadTest")(setHeadTest)
//    Wrapper("appendTest")(appendTest)
//    Wrapper("initTest")(initTest)
//    Wrapper("foldRightTest")(foldRightTest)
//    Wrapper("foldLeftTest")(foldLeftTest)
//    Wrapper("lengthTest")(lengthTest)
//    Wrapper("foldVarious")(foldVarious)
//    Wrapper("toStringTest")(toStringTest)
//    Wrapper("appendTest2")(appendTest2)
//    Wrapper("concatTest")(concatTest)
//    Wrapper("mapTest")(mapTest)
//    Wrapper("flatMapTest")(flatMapTest)
    Wrap("filterTest")(filterTest())
  }
}
