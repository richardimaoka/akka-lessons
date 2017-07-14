package my.cats

import my.wrapper.Wrap


object SemiGroupApp {
  import cats.implicits._

  def testMap(): Unit = {
    val a = Map("foo" -> Map("bar" -> 5))
    val b = Map("foo" -> Map("bar" -> 6), "baz" -> Map[String,Int]())
    println( a combine b )

    val c = Map("foo" -> List(1, 2))
    val d = Map("foo" -> List(3, 4), "bar" -> List(42))
    println( c combine d )

    val e = Map("foo" -> Map("bar" -> 5))
    val f = Map("foo" -> Map("bar" -> 6), "baz" -> Map())
    println( e ++ f )

    val g = Map("foo" -> List(1, 2))
    val h = Map("foo" -> List(3, 4), "bar" -> List(42))
    println( g ++ h )
  }

  def main(args: Array[String]): Unit ={
    Wrap("testMap")(testMap)
  }

}
