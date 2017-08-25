package my.eff

object MemoEffectApp {
  def main(args: Array[String]): Unit = {
    import cats.Eval
    import cats.implicits._
    import org.atnos.eff._, memo._
    import org.atnos.eff.syntax.memo._
    import org.atnos.eff.syntax.eval._
    import org.atnos.eff.syntax.eff._

    type S = Fx.fx2[Memoized, Eval]

    var i = 0

    def expensive[R :_memo]: Eff[R, Int] =
      memoize("key", { i += 1; 10 * 10 })

    (expensive[S] >> expensive[S]).runMemo(ConcurrentHashMapCache()).runEval.run === 100

    //"there is only one invocation" <==> (i === 1)
  }
}
