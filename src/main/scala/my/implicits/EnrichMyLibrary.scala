package my.implicits

object EnrichMyLibrary {
  def main(args: Array[String]): Unit ={
    /**
     * Note the implicit **class**
     */
    implicit class RichInt(self: Int) {
      def times(block: => Unit): Unit = {
        var n = 0
        while(n < self) {
          block
          n += 1
        }
      }
    }

    3.times {
      print("A")
    } // => AAAを表示
  }
}
