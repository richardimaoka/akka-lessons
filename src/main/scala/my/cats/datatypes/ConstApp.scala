package my.cats.datatypes

import my.wrapper.Wrap

object ConstApp {
  def intro (): Unit ={
    // The Const data type can be thought of similarly to the const function, but as a data type.
    def const[A, B](a: A)(b: => B): A = a

    // The const function takes two arguments and simply returns the first argument, ignoring the second.
    final case class Const[A, B](getConst: A)
    // Because the second type parameter is not used in the data type,
    // the type parameter is referred to as a “phantom type”.
  }

  def lensExample(): Unit = {
    /**
     * Types that contain other types are common across many programming paradigms
     * A lens can be thought of as a first class getter/setter.
     */
    trait Lens[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))
    }

    type Name = String
    case class Person(name: Name)

    val personLens = new Lens[Person, Name] {
      def get(p: Person): Name = p.name

      def set(p: Person, newName: Name): Person =
        p.copy(name = newName)
    }

    println(personLens.get(Person("Harry")))
    println(personLens.set(Person("Harry"), "Potter"))
    println(personLens.modify(Person("Harry"))(name => name + " Potter"))

    /**
     * It can be useful to have effectful modifications as well
     * (effectual -> represented by Monads, higher-kinds)
     */
    trait Lens2[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))

      def modifyOption(s: S)(f: A => Option[A]): Option[S] =
        f(get(s)).map(a => set(s, a))

      def modifyList(s: S)(f: A => List[A]): List[S] =
        f(get(s)).map(a => set(s, a))
    }

    val personLens2 = new Lens2[Person, Name] {
      def get(p: Person): Name = p.name

      def set(p: Person, newName: Name): Person =
        p.copy(name = newName)
    }

    println()
    println(personLens2.get(Person("Harry")))
    println(personLens2.set(Person("Harry"), "Potter"))

    val nameChanger: (Name) => Name = name => name + " Potter"
    println(personLens2.modify(Person("Harry"))(nameChanger))

    println(personLens2.modifyOption(Person("Harry"))(name => Some(nameChanger(name))))
    println(personLens2.modifyList(Person("Harry"))(name => List(nameChanger(name),nameChanger(name))))

    /**
     * Abstract over Option and List, using Functor
     */

    import cats.Functor
    import cats.implicits._

    trait Lens3[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S)(f: A => A): S =
        set(s, f(get(s)))

      //def modifyF[F[_]](s: S)(f: A => F[A])(implicit Functor[F]): F[S]
      def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S] =
        f(get(s)).map(a => set(s, a))
    }
    
    val personLens3 = new Lens3[Person, Name] {
      def get(p: Person): Name = p.name

      def set(p: Person, newName: Name): Person =
        p.copy(name = newName)
    }

    println()
    println(personLens3.get(Person("Harry")))
    println(personLens3.set(Person("Harry"), "Potter"))

    println(personLens3.modify(Person("Harry"))(nameChanger))

//    import cats.instances.all._
//    import cats.syntax.all._
//
//    println(personLens3.modifyF(Person("Harry"))(name => Option(nameChanger(name))))
//    println(personLens3.modifyF(Person("Harry"))(name => List(nameChanger(name),nameChanger(name))))

    
    import cats.Id
    trait Lens4[S, A] {
      def get(s: S): A

      def set(s: S, a: A): S

      def modify(s: S, a: A)(f: A => A): S =
        modifyF[Id](s)(f)

      def modifyF[F[_]: Functor](s: S)(f: A => F[A]): F[S]
    }

    import cats.data.Const

    implicit def constFunctor[X]: Functor[Const[X, ?]] =
      new Functor[Const[X, ?]] {
        // Recall Const[X, A] ~= X, so the function is not of any use to us
        def map[A, B](fa: Const[X, A])(f: A => B): Const[X, B] =
          Const(fa.getConst)
      }

    trait LensFinal[S, A] {
      def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S]

      def set(s: S, a: A): S = modify(s)(_ => a)

      def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)

      def get(s: S): A = {
        val storedValue = modifyF[Const[A, ?]](s)(a => Const(a))
        storedValue.getConst
      }
    }
  }

  def traverseExample(): Unit ={
    import cats.{Applicative, Monoid}

    trait Foldable[F[_]] {
      // Given a collection of data F[A], and a function mapping each A to a B where B has a Monoid instance,
      // reduce the collection down to a single B value using the monoidal behavior of B
      def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
    }

    trait Traverse[F[_]] {
      // Given a collection of data F[A], for each value apply the function f which returns an effectful
      // value. The result of traverse is the composition of all these effectful values.
      def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    }

    /**
     * It may be surprising to see that in fact Traverse subsumes Foldable.
     */
    trait Traverse2[F[_]] extends Foldable[F] {
      def traverse[G[_] : Applicative, A, X](fa: F[A])(f: A => G[X]): G[F[X]]

      def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
    }

    import cats.data.Const

    implicit def constApplicative[Z]: Applicative[Const[Z, ?]] =
      new Applicative[Const[Z, ?]] {
        def pure[A](a: A): Const[Z, A] = ???

        def ap[A, B](f: Const[Z, A => B])(fa: Const[Z, A]): Const[Z, B] = ???
      }

    implicit def constApplicative2[Z : Monoid]: Applicative[Const[Z, ?]] =
      new Applicative[Const[Z, ?]] {
        def pure[A](a: A): Const[Z, A] = Const(Monoid[Z].empty)

        def ap[A, B](f: Const[Z, A => B])(fa: Const[Z, A]): Const[Z, B] =
          Const(Monoid[Z].combine(fa.getConst, f.getConst))
      }

    trait Traverse3[F[_]] extends Foldable[F] {
      def traverse[G[_] : Applicative, A, X](fa: F[A])(f: A => G[X]): G[F[X]]

      def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B = {
        val const: Const[B, F[Nothing]] = traverse[Const[B, ?], A, Nothing](fa)(a => Const(f(a)))
        const.getConst
      }
    }
  }

  def main(args: Array[String]) = {
    Wrap("intro")(intro)
    Wrap("lensExample")(lensExample)
    Wrap("traverseExample")(traverseExample)
  }
}
