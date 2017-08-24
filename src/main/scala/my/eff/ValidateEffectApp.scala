package my.eff

import my.wrapper.Wrap

object ValidateEffectApp {
  def main(args: Array[String]): Unit = {
    import org.atnos.eff._, all._, syntax.all._

    /**
     * Stack declaration
     */
    type S = Fx.fx1[Validate[String, ?]]

    /**
     * Either fails first, but validate keeps execution going?
     */
    //The Validate effect is similar to the Either effect but let you accumulate failures:
    def checkPositiveInt(i: Int): Eff[S, Unit] =
      validateCheck(i >= 0, s"$i is not positive")

    def checkPositiveInts(a: Int, b: Int, c: Int): Eff[S, (Int, Int, Int)] = for {
      _ <- checkPositiveInt(a)
      _ <- checkPositiveInt(b)
      _ <- checkPositiveInt(c)
    } yield (a, b, c)

    println(checkPositiveInts(1, -3, -2).runNel.run)
    //Left(NonEmptyList(-3 is not positive, -2 is not positive))
  }
}
