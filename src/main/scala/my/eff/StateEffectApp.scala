package my.eff

import my.wrapper.Wrap

object StateEffectApp {
  def sample1(): Unit = {
    import cats.data._
    import org.atnos.eff._, all._, syntax.all._

    type S1[A] = State[Int, A]
    type S2[A] = State[String, A]

    type S = Fx.fx2[S1, S2]

    val swapVariables: Eff[S, String] = for {
    //`get` get the current state
      v1 <- get[S, Int]
      v2 <- get[S, String]
      //`put` set a new state
      _  <- put[S, Int](v2.size)
      _  <- put[S, String](v1.toString)
      w1 <- get[S, Int]
      w2 <- get[S, String]
    } yield "initial: "+(v1, v2).toString+", final: "+(w1, w2).toString

    /**
     * 2 evalState() because of FX.fx2 ?
     */
    val result = swapVariables
      .evalState(10)
      .evalState("hello")
      .run

    println(result)
    //initial: (10,hello), final: (5,10)

    /**
     * More `get`s
     */
    val swapVariables2: Eff[S, String] = for {
    //`get` get the current state
      v1 <- get[S, Int]
      v2 <- get[S, String]
      //`put` set a new state
      _  <- put[S, Int](v2.size)
      _  <- put[S, String](v1.toString)
      //_  <- put[S, String](v1.toString + v1.toString) //this still compiles hm...
      w1 <- get[S, Int]
      w2 <- get[S, String]
      w3 <- get[S, String] // this also compiles, hmmm
    } yield "initial: "+(v1, v2).toString+", final: "+(w1, w2).toString + ", " + w3

    val result2 = swapVariables2
      .evalState(10)
      .evalState("hello")
      .run

    println(result2)
    //initial: (10,hello), final: (5,10), 10


    /**
     * More `put`s
     */
    val swapVariables3: Eff[S, String] = for {
    //`get` get the current state
      v1 <- get[S, Int]
      v2 <- get[S, String]
      //`put` set a new state
      _  <- put[S, Int](v2.size)
      _  <- put[S, String](v1.toString)
      _  <- put[S, String](v1.toString + v1.toString) //this still compiles hm...
      w1 <- get[S, Int]
      w2 <- get[S, String]
      w3 <- get[S, String] // this also compiles, hmmm
    } yield "initial: "+(v1, v2).toString+", final: "+(w1, w2).toString + ", " + w3

    val result3 = swapVariables3
      .evalState(10)
      .evalState("hello")
      .run

    println(result3)
    //initial: (10,hello), final: (5,1010), 1010
  }

  def sample2(): Unit = {
    import org.atnos.eff._, all._, syntax.all._
    import cats.data.State

    type Count[A] = State[Int, A]
    type Sum[A]   = State[Int, A]
    type Mean[A]  = State[(Int, Int), A]

    type S1 = Fx.fx1[Count]
    type S2 = Fx.fx1[Sum]
    type S  = Fx.fx1[Mean]

    def count(list: List[Int]): Eff[S1, String] = for {
      _ <- put(list.size)
    } yield s"there are ${list.size} values"

    def sum(list: List[Int]): Eff[S2, String] = {
      val s = if (list.isEmpty) 0 else list.sum
      for {
        _ <- put(s)
      } yield s"the sum is $s"
    }

    def mean(list: List[Int]): Eff[S, String] = for {
      m1 <- count(list).lensState((_:(Int, Int))._1, (s: (Int,Int), i: Int) => (i, s._2))
      m2 <- sum(list).lensState((_:(Int, Int))._2, (s: (Int, Int), i: Int) => (s._1, i))
    } yield m1+"\n"+m2

    val result = mean(List(1, 2, 3)).runState((0, 0)).run
    println(result)
    //(there are 3 values
    //  the sum is 6,(3,6))
  }

  def main(args: Array[String]): Unit = {
    Wrap("sample1")(sample1)
    Wrap("sample2")(sample2)
  }
}
