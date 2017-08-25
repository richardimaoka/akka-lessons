package my.eff

object ChooseEffectApp {
  def main(args: Array[String]): Unit = {
    import org.atnos.eff._, all._, syntax.all._

    type S = Fx.fx1[Choose]

    // create all the possible pairs for a given list
    // where the sum is greater than a value
    def pairsBiggerThan[R :_choose](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
      a <- chooseFrom(list)
      b <- chooseFrom(list)
      found <- if (a + b > n) EffMonad[R].pure((a, b))
      else           zero
    } yield found

    import cats.instances.list._

    val result = pairsBiggerThan[S](List(1, 2, 3, 4), 5).runChoose.run

    println(result)
    //List((2,4), (3,3), (3,4), (4,2), (4,3), (4,4))
  }
}
