package my.cats.advanced

object TypeClassBasics {

  def typeClass1(): Unit ={
    import JsonWriterInstances._

    /**
     * def toJson[A](value: A)(implicit w: JsonWriter[A]): Json
     *
     * JsonWriter instance is passed implicitly due to the above import
     */
    Json.toJson(Person("Dave", "dave@example.com"))
    // res4: Json = JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))

  }

  def typeClass2(): Unit ={
    import JsonWriterInstances._
    import JsonSyntax._

    /**
     * def toJson(implicit w: JsonWriter[A]): Json
     *
     * both import JsonWriterInstances (which includes implicit parameters to pass to `toJson`)
     * and  import JsonSyntax (`toJson` definition)
     *
     * is needed.
     */
    Person("Dave", "dave@example.com").toJson
    /// res5: Json = JsObject(Map(name -> JsString(Dave), email -> JsString(dave@example.com)))
  }

  def main(args: Array[String]): Unit = {
  }
}
