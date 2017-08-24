package my.eff

object OptionEffectApp {
  def main(args: Array[String]): Unit ={
    import org.atnos.eff._, all._, syntax.all._

    /**
     * Stack declaration
     */
    type S = Fx.fx1[Option]

    // compute with this stack
    val map: Map[String, Int] =
      Map("key1" -> 10, "key2" -> 20)

    // get 2 keys from the map and add the corresponding values
    def addKeys(key1: String, key2: String): Eff[S, Int] = for {
      a <- fromOption(map.get(key1))
      b <- fromOption(map.get(key2))
    } yield a + b

    val result1 = addKeys("key1", "key2").runOption.run
    val result2 = addKeys("key1", "missing").runOption.run

    println(result1)
    // Some(30)
    println(result2)
    // None

    println(map.get("key1"))
    println(map.get("key2"))
    //Some(10)
    //Some(20)

    /**
     *   /** create an Option effect from a single Option value */
     *   def fromOption[R :_option, A](o: Option[A]): Eff[R, A] =
     *     send[Option, R, A](o)
     */
    println(fromOption(map.get("key1"))) // : Eff[R, String]
    //ImpureAp(Unions(UnionTagged(Some(10),1),Vector()),<function1>,Last(None))

    println(fromOption(map.get("key1")).runOption)
    //Impure(NoEffect(Vector(10)),<function1>,Last(None))

    println(fromOption(map.get("key1")).runOption.run)
    //Some(10)

    //println(fromOption(map.get("key1")).runOption.runEval)
    //compilation error
  }
}
