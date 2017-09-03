package my.cats.officialdoc

import cats.free.Free
import cats.mtl._
import my.wrapper.Wrap

/**
 * Imagine you have a key-value store.
 * You want to do something like below, but in a functional way
 *
 *   put("toto", 3)
 *   get("toto") // returns 3
 *   delete("toto")
 */

object FreeMonadApp {

  //We need to create an ADT to represent our key-value operations:
  sealed trait KVStoreA[A] //KVStore*A*
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Delete(key: String) extends KVStoreA[Unit]

  def firstAttempt(): Unit ={
    /**
     * The type alias name iks KVStore, not KVStore*A*
     *
     * and also note it is Free[KVStoreA, A], not Free[KVStoreA[A], A]
     * (i.e.) the first type parameter to Free is higher-kind.
     */
    type KVStore[A] = Free[KVStoreA, A]

    import cats.free.Free.liftF

    // Put returns nothing (i.e. Unit).
    def put[T](key: String, value: T): KVStore[Unit] =
      liftF[KVStoreA, Unit](Put[T](key, value))

    // Get returns a T value.
    def get[T](key: String): KVStore[Option[T]] =
      liftF[KVStoreA, Option[T]](Get[T](key))

    // Delete returns nothing (i.e. Unit).
    def delete(key: String): KVStore[Unit] =
      liftF(Delete(key))

    // Update composes get and set, and returns nothing.
    def update[T](key: String, f: T => T): KVStore[Unit] =
      for {
        vMaybe <- get[T](key)
        _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()

    //Put(key,1)
    println(Put("key", 1))

    //Free(...)
    println(put("key", 1)) //liftF[KVStoreA, Unit](Put[Int]("key", 1))

    //Interesting, how is [T] of get[T] inferred?
    //Free(...)
    println(get("key"))    //liftF[KVStoreA, Option[T]](Get[T](key))


    /**
     * Now that we can construct KVStore[_] values we can use our DSL to write “programs”
     * using a for-comprehension
     */
    def program: KVStore[Option[Int]] = //Free[KVStoreA, Option[Int]]
      for {
        //KVStore[Unit]        = Free[KVStoreA, Unit]
        _ <- put("wild-cats", 2)
        //liftF[KVStoreA, Unit](Put[T]("wild-cats", 2))

        //KVStore[Unit]        = Free[KVStoreA, Unit]
        _ <- update[Int]("wild-cats", (_ + 12))
        //get and, if it is there, put

        //KVStore[Unit]        = Free[KVStoreA, Unit]
        _ <- put("tame-cats", 5)
        //liftF[KVStoreA, Unit](Put[Int]("tame-cats", 5))

        //KVStore[Option[Int]] = Free[KVStoreA, Option[Int]]
        n <- get[Int]("wild-cats")
        //liftF[KVStoreA, Option[Int]](Get[Int]("wild-cats"))

        //KVStore[Unit]        = Free[KVStoreA, Unit]
        _ <- delete("tame-cats")
        //liftF(Delete(key))

      } yield n
    /**
     * By itself, this DSL only represents a sequence of operations
     * (defined by a recursive data structure);
     * it doesn’t produce anything.
     */

    import cats.arrow.FunctionK
    import cats.{Id, ~>}
    import scala.collection.mutable

    // the program will crash if a key is not found,
    // or if a type is incorrectly specified.
    def impureCompiler: KVStoreA ~> Id  = new (KVStoreA ~> Id) {
      /**
       * F ~> G
       *
       * Natural transformations go between types like F[_] and G[_]
       * This particular transformation would be written as FunctionK[F,G]
       * or as done here using the symbolic alternative as F ~> G
       */
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

    println(s"result: ${result}")
    // put(wild-cats, 2)
    // get(wild-cats)
    // put(wild-cats, 14)
    // put(tame-cats, 5)
    // get(wild-cats)
    // delete(tame-cats)
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

    val resultPure: (Map[String, Any], Option[Int])
      = program.foldMap(pureCompiler).run(Map.empty).value

    println(s"resultPure: ${resultPure}")
  }

  def secondAttempt(): Unit = {
    import cats.data.EitherK
    import cats.free.{Free}
    import cats.InjectK
    import cats.{Id, ~>}
    import scala.collection.mutable.ListBuffer

    /* Handles user interaction */
    sealed trait Interact[A]
    case class Ask(prompt: String) extends Interact[String]
    case class Tell(msg: String) extends Interact[Unit]

    /* Represents persistence operations */
    sealed trait DataOp[A]
    case class AddCat(a: String) extends DataOp[Unit]
    case class GetAllCats() extends DataOp[List[String]]

    // Once the ADTs are defined
    // we can formally state that a Free program is the EitherK of it’s Algebras.
    type CatsApp[A] = EitherK[DataOp, Interact, A]

    //Lift previously-defined ADT (algebraic data types) to the `Free` context
    class Interacts[F[_]](implicit I:InjectK[Interact, F]) {
      def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
      def ask(prompt: String): Free[F, String] = Free.inject[Interact, F](Ask(prompt))
    }

    //For implicit resolution, add it in the companion object
    object Interacts {
      implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
    }

    //Lif previously-defined ADT (algebraic data types) to the `Free` context
    class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
      def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
      def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
    }

    //For implicit resolution, add it in the companion object
    object DataSource {
      implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
    }

    //ADTs are now easily composed and trivially intertwined inside monadic contexts.
    def program(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {

      import I._, D._

      for {
        //def ask(prompt: String): Free[CatsApp, String]
        cat <- ask("What's the kitty's name?")
        //= Free.inject[Interact, CatsApp](Ask("What's the kitty's name?"))

        //def addCat(a: String): Free[CatsApp, Unit]
        _ <- addCat(cat)
        //Free.inject[DataOp, CatsApp](AddCat(a))

        //def getAllCats: Free[CatsApp, List[String]]
        cats <- getAllCats
        //Free.inject[DataOp, CatsApp](GetAllCats())

        //def tell(msg: String): Free[CatsApp, Unit]
        _ <- tell(cats.toString)
        //Free.inject[Interact, CatsApp](Tell(cats.toString))

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

    import DataSource._, Interacts._

    val evaled: Unit = program.foldMap(interpreter)
    // What's the kitty's name?
    //   // wait for user input
    // List(snuggles)
    // evaled: Unit = ()
  }

  def inTheory(): Unit = {
    sealed abstract class Free[F[_], A]
    case class Pure[F[_], A](a: A) extends Free[F, A]
    case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]


    /**
     * In Cats, FlatMapped is defined as follows:
     */
    case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B])
      extends Free[S, B]
  }

  def freeT(): Unit ={
    import cats.free._
    // import cats.free._

    import cats._
    // import cats._

    import cats.data._
    // import cats.data._

    /* A base ADT for the user interaction without state semantics */
    sealed abstract class Teletype[A] extends Product with Serializable
    // defined class Teletype

    final case class WriteLine(line : String) extends Teletype[Unit]
    // defined class WriteLine

    final case class ReadLine(prompt : String) extends Teletype[String]
    // defined class ReadLine

    type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
    // defined type alias TeletypeT

    type Log = List[String]
    // defined type alias Log

    type TeletypeState[A] = State[List[String], A]
    // defined type alias TeletypeState

    /** Smart constructors, notice we are abstracting over any MonadState instance
     *  to potentially support other types beside State
     */
    object TeletypeOps {
      def writeLine(line : String) : TeletypeT[TeletypeState, Unit] =
        FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))
      def readLine(prompt : String) : TeletypeT[TeletypeState, String] =
        FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))
      def log(s : String) : TeletypeT[TeletypeState, Unit] =
        FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))
    }
    // defined object TeletypeOps

    def program : TeletypeT[TeletypeState, Unit] = {
      for {
        userSaid <- TeletypeOps.readLine("what's up?!")
        _ <- TeletypeOps.log(s"user said : $userSaid")
        _ <- TeletypeOps.writeLine("thanks, see you soon!")
      } yield ()
    }
    // program: TeletypeT[TeletypeState,Unit]

    def interpreter = new (Teletype ~> TeletypeState) {
      def apply[A](fa : Teletype[A]) : TeletypeState[A] = {
        fa match {
          case ReadLine(prompt) =>
            println(prompt)
            val userInput = "hanging in here" //scala.io.StdIn.readLine()
            StateT.pure[Eval, List[String], A](userInput)
          case WriteLine(line) =>
            StateT.pure[Eval, List[String], A](println(line))
        }
      }
    }
    // interpreter: Teletype ~> TeletypeState

    import TeletypeOps._
    // import TeletypeOps._

    val state = program.foldMap(interpreter)
    // state: TeletypeState[Unit] = cats.data.StateT@29d862d5

    val initialState = Nil
    // initialState: scala.collection.immutable.Nil.type = List()

    val (stored, _) = state.run(initialState).value
    // what's up?!
    // thanks, see you soon!
    // stored: List[String] = List(user said : hanging in here)
  }

  def main(args: Array[String]): Unit = {
    Wrap("firstAttempt")(firstAttempt)
    Wrap("secondAttempt")(secondAttempt)

  }
}
