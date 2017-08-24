package my.eff

object ReaderEffectApp {
  def main(args: Array[String]): Unit = {
    import org.atnos.eff._, all._, syntax.all._
    import cats.data._

    case class Conf(host: String, port: Int)

    type R1[A] = Reader[Int, A]
    type R2[A] = Reader[Conf, A]

    type S = Fx.fx2[R1, R2]

    def getPort[R](implicit r: Reader[Int, ?] |= R): Eff[R, String] = for {
      p1 <- ask[R, Int] //The main method is `ask` to get the current environment (or “configuration”)
    } yield "the port is " + p1

    val result = getPort[S].translateReader((_: Conf).port).runReader(Conf("prod", 80)).run
    println(result)
    //the port is 80
  }
}
