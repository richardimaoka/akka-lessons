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


    /**
     * You can also use implicit conversion for enrich-my-library
     * According to https://dwango.github.io/scala_text/implicit.html#pimp-my-library
     * the above implicit **class** is preferred
     */
    class RichString(val src: String) {
      def smile: String = src + ":-)"
    }

    implicit def enrichString(arg: String): RichString = new RichString(arg)
    println( "Hi, ".smile )
  }
}
