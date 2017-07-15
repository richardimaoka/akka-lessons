package my.implicits

/**
 * Scala Option implicit conversion - Bad practice or missing feature?
 * https://stackoverflow.com/questions/27590756/scala-option-implicit-conversion-bad-practice-or-missing-feature
 *
 * So seems like the danger is realized (especially? or often?)
 * when converting into a widely-used existing classes.
 * https://dwango.github.io/scala_text/implicit.html
 *   本来Booleanしか渡せないはずのif式にIntを渡すことができています。
 *   ただし、この使い方はあまり良いものではありません。
 *   上の例をみればわかる通り、implicit conversionを定義することで、コンパイラにより、
 *   本来はif式の条件式にはBoolean型の式しか渡せないようにチェックしているのを通りぬけることができてしまうからです
 */
object ImplicitConversions {

  def main(args: Array[String]): Unit = {
    /**
     * I represented my data model as case classes typing values that may be null as Option.
     */
    case class Document(id: Long, title: String, subtitle: Option[String])

    /**
     * Now I try to instantiate the case class.
     * But NOPE! This doesn't work, I have to wrap the optional value in a Some.
     */
    //Document(123, "The Title", "Subtitle") // Doesn't work

    Document(123, "The Title", Some("Subtitle")) // Works
    /**
     * Scala is very clever about types in general,
     * but why is it not self-evident that a hard coded literal,
     * or (any string for that matter) is a different than null/None?
     * I was able to fix this and make Scala "more clever" by adding this implicit conversion
     */

    implicit def autoSome[T](any:T) = Some(any)
    Document(123, "The Title", "Subtitle") // Now it works!
    /**
     * Question: Am I the only one that the language should provide
     * implicit conversion T -> Some(T) out of the box?
     * Or is there any gotchas that I'm not aware of about having so
     * broad implicit everywhere by default?
     */

    /**
     * The answer shows why the above expectation is dangerous
     * ... actually it doesn't ..
     */
    answer()
  }

  def answer(): Unit = {
    implicit def autoSome[T](any:T) = Some(any)

    implicit class OptDouble(opt: Option[Double]) {
      def *(x: Double) = Some((opt getOrElse 0.0) * x)
      def ^(x: Double) = Some(Math.pow(opt getOrElse 1.0, x))
    }

    val q: Double = 2

    /**
     * Hmmm!!!??? This doesn't even compile... I don't know
     * what the person who gave this answer wanted to do.
     *
     * :(
     */
    //val z = q^4.5

    //println(z)
  }

}
