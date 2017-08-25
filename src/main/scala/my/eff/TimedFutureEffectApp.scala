package my.eff

import my.wrapper.Wrap

object TimedFutureEffectApp {
  def sample1(): Unit = {
    import org.atnos.eff._
    import org.atnos.eff.future._
    import org.atnos.eff.syntax.all._

    import scala.concurrent._, duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    type R = Fx.fx2[TimedFuture, Option]

    val action: Eff[R, Int] =
      for {
        // create a value now
        a <- Eff.pure[R, Int](1)

        // evaluate a value later, on some other thread pool, and continue when it's finished
        b <- futureDelay[R, Int](1)
      } yield b

    implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext
    import org.atnos.eff.syntax.future._

    val result = Await.result(action.runOption.runSequential, 1 second)

    println(result)
    // Some(1)

    println(Eff.pure[R, Int](1))
    // Pure(1,Last(None))

    println(futureDelay[R, Int](1))
    // ImpureAp(Unions(UnionTagged(TimedFuture(org.atnos.eff.FutureCreation$$Lambda$6244/1014968897@42f052a5,None),1),Vector()),<function1>,Last(None))
  }

  def sample2(): Unit ={
    import cats.implicits._
    import org.atnos.eff._, future._, all._
    import org.atnos.eff.syntax.all._
    import org.atnos.eff.syntax.future._
    import scala.concurrent._, duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    var i = 0

    def expensive[R :_Future :_memo]: Eff[R, Int] =
      futureMemoized[R, Int]("key", futureDelay[R, Int] { i += 1; 10 * 10})

    type S = Fx.fx2[Memoized, TimedFuture]

    implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

    val futureMemo: Future[Int] =
      (expensive[S] >> expensive[S]).runFutureMemo(ConcurrentHashMapCache()).runSequential

    val result = Await.result(futureMemo, 1.second)
    println(result)
    // 100

    //"there is only one invocation" <==> (i === 1)
  }

  def main(args: Array[String]): Unit = {
    Wrap("sample1")(sample1)
    Wrap("sample2")(sample2)
  }
}
