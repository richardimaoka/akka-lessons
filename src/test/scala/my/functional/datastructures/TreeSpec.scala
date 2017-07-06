package my.functional.datastructures

import org.scalatest._

class TreeSpec extends WordSpec with Matchers {
  import Tree._
  
  "Tree" must {
    "do basics" in {
      println( Leaf(1) )
      println( Branch( Leaf(1), Leaf(2)) )
      println( Branch( Leaf(1), Branch(Leaf(2),Leaf(2))))
      println( Branch( Leaf(1), Branch(Leaf(2),Branch(Leaf(2),Leaf(2)))))

    }

    "calculate size" in {
      println(Tree.size(Branch( Leaf(1), Branch(Leaf(2),Branch(Leaf(2),Leaf(2))))))
    }
  }

}