package my.functional.monads

import my.wrapper.Wrap

import scala.language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  /**
   * So called unzip
   */
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  /**
   *
   */
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))   //if map were a method, it is like fa map (Left(_))
    case Right(fb) => map(fb)(Right(_)) //if map were a method, it is like fb map (Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

object FunctorTest {
  import Functor._

  def listFunctorTest(): Unit = {
    println(listFunctor.map(List(1,2,3))(x => (x + 3).toString() + " bah!"))

    //distribute is same as unzip
    println(listFunctor.distribute(List((1,2), (3,4))))

    println(listFunctor.codistribute(Right(List(1,2,3,4,5))))
  }

  def main(args: Array[String]): Unit = {
    Wrap("listFunctorTest")(listFunctorTest)
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  /**
   * for{
   *   a <- ma
   *   b <- mb
   * } yield f(a,b)
   */

  /**
   * The argument is in List
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(
      unit(List[A]()) //init value: unit(List[A]()) = F( List[A]() )
    )(
      (ma, mla) => map2(ma, mla)(_ :: _) // a1 :: a2 :: ... :: List[A]()
      /**
       * for{
       *   a <- ma
       *   la <- mla
       * } yield (a :: la)
       *
       * for {
       *   a <- List(1,2)
       *   la <- F(Nil)
       * } yield (a :: la)
       */
    )

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(
      unit(List[B]()) //init value: unit(List[B]()) = F( List[B]() )
    )(
      (a, mlb) => map2(f(a), mlb)(_ :: _) // f(a1) :: f(a2) :: ..... :: List[B]()
    )

  // Using `sequence` and the `List.fill` function of the standard library:
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))


  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.

  // Recursive version:
  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)



  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)


  /*
  For `Par`, `filterM` filters a list, applying the functions in
  parallel; for `Option`, it filters a list, but allows
  the filtering function to fail and abort the filter
  computation; for `Gen`, it produces a generator for
  subsets of the input list, where the function `f` picks a
  'weight' for each element (in the form of a
  `Gen[Boolean]`)
  */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }


//  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
//    ms.foldRight(unit(List[A]()))((x,y) =>
//      compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))
}

object Monad {
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
    /**
     *  def flatMap[B](f: A => Option[B]): Option[B] =
     *    if (isEmpty) None else f(this.get)
     */
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }
}

object MonadTest {
  import Monad._

  def unitTest(): Unit = {
    println(listMonad.unit(List[Int]()))
    println(optionMonad.unit(List[Int]()))
  }

  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.
  def replicateTest: Unit ={
    println(optionMonad.replicateM(5, Some(2)))
    println(listMonad.replicateM(3, List(1)))
    println(listMonad.replicateM(3, List(2,3)))
    println(List.fill(3)(List(2,3)))

    val x = for {
      a <- List(1,2)
      la <- List(List[Int]())
    } yield (a :: la)

    println(x)
  }

  def sequenceTest: Unit = {
    println( optionMonad.sequence(List(Some(2), None, Some(4))) )
    println( optionMonad.sequence(List(Some(2), Some(3), Some(4))) )

    println( streamMonad.sequence(List(Stream(1,2), Stream(1,2))))

    // For `List`, the `replicateM` function will generate a list of lists.
    // It will contain all the lists of length `n` with elements selected from the
    // input list.
    // For `Option`, it will generate either `Some` or `None` based on whether the
    // input is `Some` or `None`. The `Some` case will contain a list of length `n`
    // that repeats the element in the input `Option`.
    // The general meaning of `replicateM` is described very well by the
    // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
    // `n` times and gathers the results in a single value, where the monad `M`
    // determines how values are actually combined.
    val lma = List(List(2,3), List(2,3), List(2,3))
    val result = lma.foldRight(
      listMonad.unit(List[Int]()) //init value: unit(List[A]()) = F( List[A]() )
    )(
      (ma, mla) => {
        println(s"map2 called for ma = ${ma} mla = ${mla}")
        listMonad.map2(ma, mla)(_ :: _)
      } // a1 :: a2 :: ... :: List[A]()
      /**
       * map2 is equivalent to:
       * for{
       *   a <- ma
       *   la <- mla
       * } yield (a :: la)
       *
       *
       * So, firstly:
       *
       * for {
       *   a <- List(2,3)
       *   la <- List(List())
       * } yield (a :: la)
       *
       *   = List(2), List(3)
       *
       * then:
       *
       * for {
       *   a <- List(2,3)
       *   la <- List(List(2), List(3))
       * } yield (a :: la)
       *
       *   = List(List(2, 2), List(2, 3), List(3, 2), List(3, 3))
       *   //it is just double loop ... see how the are expanded
       *
       *
       * finally:
       *
       * for {
       *   a <- List(2,3)
       *   la <- List(List(2, 2), List(2, 3), List(3, 2), List(3, 3))
       * } yield (a :: la)
       *
       *   = List(List(2, 2, 2), List(2, 2, 3), List(2, 3, 2), List(2, 3, 3), List(3, 2, 2), List(3, 2, 3), List(3, 3, 2), List(3, 3, 3))
       *   //it is just double loop ... see how the are expanded
       *
       */
    )
    println(result)
  }

  def main(args: Array[String]): Unit = {
    Wrap("unitTest")(unitTest)
    Wrap("replicateTest")(replicateTest)
    Wrap("sequenceTest")(sequenceTest)
  }
}