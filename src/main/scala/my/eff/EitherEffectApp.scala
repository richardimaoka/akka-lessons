package my.eff

import my.wrapper.Wrap

object EitherEffectApp {
  def sample1(): Unit ={
    import org.atnos.eff._, all._, syntax.all._

    /**
     * Stack declaration
     */
    type S = Fx.fx1[String Either ?]

    // compute with this stack
    val map: Map[String, Int] =
      Map("key1" -> 10, "key2" -> 20)

    // get 2 keys from the map and add the corresponding values
    def addKeys(key1: String, key2: String): Eff[S, Int] = for {
      a <- optionEither(map.get(key1), s"'1: $key1' not found")
      b <- optionEither(map.get(key2), s"'2: $key2' not found")
    } yield a + b

    val result1 = addKeys("key1", "key2").runEither.run
    val result2 = addKeys("key1", "missing").runEither.run
    val result3 = addKeys("missing", "key2").runEither.run

    println(result1)
    //Right(30)
    println(result2)
    //Left('2: missing' not found) - failed once and stopped
    println(result3)
    //Left('1: missing' not found) - failed once and stopped (i.e.) optionEither for key2 is was not evaluated
  }

  def sample2(): Unit ={
    import org.atnos.eff._, all._, syntax.all._

    /**
     * Stack declaration
     */
    // equivalent to = Fx.fx1[String Either ?]
    type S = Fx.fx1[Either[String, ?]]

    // compute with this stack
    val map: Map[String, Int] =
      Map("key1" -> 10, "key2" -> 20)

    // get 2 keys from the map and add the corresponding values
    def addKeys(key1: String, key2: String): Eff[S, Int] = for {
      a <- optionEither(map.get(key1), s"1: '$key1' not found")
      b <- optionEither(map.get(key2), s"2: '$key2' not found")
    } yield a + b

    val result1 = addKeys("key1", "key2").runEither.run
    val result2 = addKeys("key1", "missing").runEither.run
    val result3 = addKeys("missing", "key2").runEither.run

    println(result1)
    //Right(30)
    println(result2)
    //Left('2: missing' not found)
    println(result3)
    //Left('1: missing' not found) - failed once and stopped (i.e.) optionEither for key2 is was not evaluated

  }

  def catchLeftSample(): Unit ={
    import org.atnos.eff._, all._, syntax.all._

    case class TooBig(value: Int)
    type E = Fx.fx1[TooBig Either ?]

    def test(i: Int): Eff[E, Int] = {
      val value: Eff[E, Int] =
        if (i > 5) left[E, TooBig, Int](TooBig(i))
        else       right[E, TooBig, Int](i)

      //A catchLeft method can also be used to intercept an error and possibly recover from it:
      val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) {
        case TooBig(k) =>
          if (k < 10) right[E, TooBig, Int](k)
          else        left[E, TooBig, Int](TooBig(k))
      }
      action
    }

    println(test(11).runEither.run)
    // Left(TooBig(11))

    //recovered by the `action` above, which recovers `value` < 10
    println(test(7).runEither.run)
    // Right(7)
    println(test(5).runEither.run)
    // Right(5)
    println(test(4).runEither.run)
    // Right(4)
  }

  def main(args: Array[String]): Unit = {
    Wrap("sample1")(sample1)
    Wrap("sample2")(sample1)
    Wrap("catchLeftSample")(catchLeftSample)
  }
}
