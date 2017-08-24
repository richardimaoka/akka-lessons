package my.eff

object EvalEffectApp {
  def main(args: Array[String]): Unit ={
    import org.atnos.eff._, all._, syntax.all._

    //runEval: Eff[U, A] to just execute the computations
    println(delay(1 + 1).runEval)
    //Impure(NoEffect(Vector(2)),<function1>,Last(None))

    println(delay(1 + 1).runEval.run)

    val a = delay{println("delayed"); 1 + 1}
    println(a.runEval.run)
    println(a.runEval.run)
    println(a.runEval.run)

    val b = delay{println("delayed"); 1 + 1}
  }
}
