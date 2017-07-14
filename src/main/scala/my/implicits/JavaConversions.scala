package my.implicits

/**
 * http://xuwei-k.hatenablog.com/entry/20160327/1459108797
 * https://gist.github.com/xuwei-k/8870ea35c4bb6a4de05c
 *
 * https://github.com/scala/scala.github.com/pull/509
 * https://github.com/scala/scala/pull/5064
 */

object JavaConversions {
  def notWorking(): Unit = {
    case class Foo(s: String)

    val map: Map[Foo, String] = Map(
      Foo("a") -> "a",
      Foo("b") -> "b"
    )

    /**
     * Of course this is in compilation error
     *   [error]  found   : String("a")
     *   [error]  required: Foo
     */
    // val v = map.get("a") // should be a type error, actually returns null
  }

  def main(args: Array[String]): Unit = {
    import scala.collection.JavaConversions._

    case class Foo(s: String)

    val map: Map[Foo, String] = Map(
      Foo("a") -> "a",
      Foo("b") -> "b"
    )

    // trap trap trap trap!!!
    // Just that unexpected conversion happening here
    val v = map.get("a") // should be a type error, actually returns null

    println(v) //null
  }
}
