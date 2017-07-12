package my.cats


object SemiGroupApp {
  import cats.implicits._

  def main(args: Array[String]): Unit ={
    println(
      Map("foo" -> Map("bar" -> 5))
        .combine(
          Map("foo" -> Map("bar" -> 6), "baz" -> Map())
        )
    )

    println(
      Map("foo" -> List(1, 2))
        .combine(
          Map("foo" -> List(3, 4), "bar" -> List(42))
        )
    )

    println(
      Map("foo" -> Map("bar" -> 5))
        ++ Map("foo" -> Map("bar" -> 6), "baz" -> Map())
    )

    println(
      Map("foo" -> List(1, 2))
        ++ Map("foo" -> List(3, 4), "bar" -> List(42))
    )

  }

}
