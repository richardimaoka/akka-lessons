package my.eff

object ListEffectApp {
  def main(args: Array[String]): Unit = {
    import org.atnos.eff._, all._, syntax.all._

    type S = Fx.fx1[List]

    // create all the possible pairs for a given list
    // where the sum is greater than a value
    def pairsBiggerThan[R :_list](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
      a <- values(list:_*)
      b <- values(list:_*)
      found <- if (a + b > n) singleton((a, b))
      else empty
    } yield found

    val result = pairsBiggerThan[S](List(1, 2, 3, 4), 5).runList.run
    println(result)
    //List((2,4), (3,3), (3,4), (4,2), (4,3), (4,4))
  }
}
