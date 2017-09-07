package my.cats.datatypes

import cats.implicits._

object FreeMonadApp {

  def freeMonadATDs(): Unit = {
    import cats.data.EitherK
    import cats.free.Free
    import cats.{Id, InjectK, ~>}
    import scala.collection.mutable.ListBuffer

    /* Handles user interaction */
    sealed trait Interact[A]
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]

    /* Represents persistence operations */
    sealed trait DataOp[A]
    case class AddCat(a: String) extends DataOp[Unit]
    case class GetAllCats() extends DataOp[List[String]]

    type CatsApp[A] = EitherK[DataOp, Interact, A]

    class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
      def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
      def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
    }

    object Interacts {
      implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
    }

    class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
      def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
      def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
    }

    object DataSource {
      implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
    }


    def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {
      import I._, D._

      for {
        cat <- ask("What's the kitty's name?")
        _ <- addCat(cat)
        cats <- getAllCats
        _ <- tell(cats.toString)
      } yield ()
    }

    object ConsoleCatsInterpreter extends (Interact ~> Id) {
      def apply[A](i: Interact[A]) = i match {
        case Ask(prompt) =>
          println(prompt)
          readLine()
        case Tell(msg) =>
          println(msg)
      }
    }

    object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

      private[this] val memDataSet = new ListBuffer[String]

      def apply[A](fa: DataOp[A]) = fa match {
        case AddCat(a) => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      }
    }

    val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter

    val evaled: Unit = program.foldMap(interpreter)
    println(evaled)
  }


  def main(args: Array[String]): Unit = {

    /**
     * Prepare operations in algebraic data types
     */
    sealed trait KVStoreA[A]
    case class Put[T](key: String, value: T) extends KVStoreA[Unit]
    case class Get[T](key: String) extends KVStoreA[Option[T]]
    case class Delete(key: String) extends KVStoreA[Unit]

    import cats.free.Free

    /**
     * Describe operations in methods returning Free monads
     */
    type KVStore[A] = Free[KVStoreA, A]
    import cats.free.Free.liftF

    /**
     * Lift an `F[A]` value into the free monad.
     * def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)
     */
    // Put returns nothing (i.e. Unit).
    def put[T](key: String, value: T): KVStore[Unit] = //Free[KVStoreA, Unit]
      liftF[KVStoreA, Unit](Put[T](key, value))

    // Get returns a T value.
    def get[T](key: String): KVStore[Option[T]] = //Free[KVStoreA, Option[T]]
      liftF[KVStoreA, Option[T]](Get[T](key))

    // Delete returns nothing (i.e. Unit).
    def delete(key: String): KVStore[Unit] = //Free[KVStoreA, Unit]
      liftF(Delete(key))

    // Update composes get and set, and returns nothing.
    def update[T](key: String, f: T => T): KVStore[Unit] = //Free[KVStoreA, Option[T]]
      for {
        vMaybe <- get[T](key)
        _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()

    /**
     * Build a program using for comprehension
     *
     * As you may have understood now, Free[_] is used to create an embedded DSL.
     * By itself, this DSL only represents a sequence of operations
     * (defined by a recursive data structure); it doesn’t produce anything.
     */
    def program: KVStore[Option[Int]] =
      for {
        _ <- put("wild-cats", 2)
        _ <- update[Int]("wild-cats", (_ + 12))
        _ <- put("tame-cats", 5)
        n <- get[Int]("wild-cats")
        _ <- delete("tame-cats")
      } yield n


    /**
     * Write a compiler for your program
     */
    import cats.arrow.FunctionK
    import cats.{Id, ~>}
    import scala.collection.mutable

    // the program will crash if a key is not found,
    // or if a type is incorrectly specified.
    def impureCompiler: KVStoreA ~> Id  = // type ~>[F[_], G[_]] = arrow.FunctionK[F, G]
      new (KVStoreA ~> Id) {

        // a very simple (and imprecise) key-value store
        val kvs = mutable.Map.empty[String, Any]

        def apply[A](fa: KVStoreA[A]): Id[A] =
          fa match {
            case Put(key, value) =>
              println(s"put($key, $value)")
              kvs(key) = value
              ()
            case Get(key) =>
              println(s"get($key)")
              kvs.get(key).map(_.asInstanceOf[A])
            case Delete(key) =>
              println(s"delete($key)")
              kvs.remove(key)
              ()
          }
      }

    val result: Option[Int] = program.foldMap(impureCompiler)
    // put(wild-cats, 2)
    // get(wild-cats)
    // put(wild-cats, 14)
    // put(tame-cats, 5)
    // get(wild-cats)
    // delete(tame-cats)

    println(result)
    // result: Option[Int] = Some(14)


    import cats.data.State

    type KVStoreState[A] = State[Map[String, Any], A]
    val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
      def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
        fa match {
          case Put(key, value) => State.modify(_.updated(key, value))
          case Get(key) =>
            State.inspect(_.get(key).map(_.asInstanceOf[A]))
          case Delete(key) => State.modify(_ - key)
        }
    }

    val result2: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value
    println(result2)
    // result: (Map[String,Any], Option[Int]) = (Map(wild-cats -> 14),Some(14))
  }
}
