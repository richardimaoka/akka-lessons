package my.eff

object WriterEffectApp {
  def main(args: Array[String]): Unit = {
    import org.atnos.eff._, all._, syntax.all._
    import cats.data.Writer
    import java.io.PrintWriter
    import scala.io

    type S = Fx.fx1[Writer[String, ?]]

    val action: Eff[S, Int] = for {
      a <- pure[S, Int](1)
      _ <- tell("first value "+a)
      b <- pure[S, Int](2)
      _ <- tell("second value "+b)

    } yield a + b

    // define a fold to output values
    def fileFold(path: String) = new RightFold[String, Unit] {
      type S = PrintWriter
      val init: S = new PrintWriter(path)

      def fold(a: String, s: S): S =
      { s.println(a); s }

      def finalize(s: S): Unit =
        s.close
    }

    action.runWriterFold(fileFold("target/log")).run
    val result = io.Source.fromFile("target/log").getLines.toList

    println(result)
  }
}
