package my.cats.officialdoc

object ConstApp {

  def constTest(): Unit = {
    /**
     * At first glance Const seems like a strange data type.
     * It has two type parameters, yet only stores a value of the first type
     */
    def const[A, B](a: A)(b: => B): A = a

    // ADT (algebraic data type)
    final case class Const[A, B](getConst: A) //B is referred to as a “phantom type”.
  }

  def lensFirst(): Unit = {
    /**
     * A lens can be thought of as a first class getter/setter.
     * A Lens[S, A] is a data type that knows how to get an A out of an S, or set an A in an S
     */
    trait Lens[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))
    }
  }

  def lensSecond(): Unit = {
    trait Lens[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))

      /**
       * It can be useful to have effectful modifications as well
       * - perhaps our modification can fail (Option) or can return several values (List).
       */
      def modifyOption(s: S)(f: A => Option[A]): Option[S] =
        f(get(s)).map(a => set(s, a))
      /**
       * f(get(s))      : Option[A]
       * a => set(s, a) : A => S
       * f(get(s)).map(a => set(s, a)): Option[S]
       */

      def modifyList(s: S)(f: A => List[A]): List[S] =
        f(get(s)).map(a => set(s, a))
      /**
       * f(get(s))      : List[A]
       * a => set(s, a) : A => S
       * f(get(s)).map(a => set(s, a)): List[S]
       */
    }
  }

  def lensThird(): Unit = {
    import cats.Functor
    import cats.implicits._

    trait Lens[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))

      /**
       * Note that both modifyOption and modifyList share the exact same implementation.
       * If we look closely, the only thing we need is a map operation on the data type.
       *
       *   ==> map operation .... Functor!!!
       *
       * Being good functional programmers, we abstract.
       */
      def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S] =
        f(get(s)).map(a => set(s, a))
      /**
       * f(get(s))      : F[A]
       * a => set(s, a) : A => S
       * f(get(s)).map(a => set(s, a)): F[S]
       */
    }
  }

  def lensFourth(): Unit = {
    import cats.Functor
    import cats.implicits._

    import cats.Id
    /**
     * We can redefine modify in terms of modifyF by using cats.Id.
     * We can also treat set as a modification that simply ignores the current value.
     * Due to these modifications however,
     * we must leave modifyF abstract since having it defined in terms of set
     * would lead to infinite circular calls.
     */
    trait Lens[S, A] {
      def get(s: S): A

      //in terms of modify
      def set(s: S, a: A): S = modify(s)(_ => a)

      //in terms of modifyF
      def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)
      /**
       * Id is a higher-kind but actually Id[A] == A
       * So, modifyF[Id] is without higher-kind, i.e. `modify`
       */

      def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S]
    }
  }

  def main(args: Array[String]): Unit = {

  }

}
