package my.cats.advanced

// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json

// The "serialize to JSON" behavior is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

/*******************************************************
 * There are two common ways of specifying an interface:
 *   1. Interface Objects and
 *   2. Interface Syntax.
 *
 * This Json object is 1. Interface Object
 *******************************************************/
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object JsonWriterInstances {

  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }

  implicit val intJsonWriter = new JsonWriter[Int] {
    def write(value: Int): Json =
      JsNumber(value)
  }

  implicit val doubleJsonWriter = new JsonWriter[Double] {
    def write(value: Double): Json =
      JsNumber(value)
  }

  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }
  // etc...
}

/*******************************************************
 * There are two common ways of specifying an interface:
 *   1. Interface Objects and
 *   2. Interface Syntax.
 *
 * This JsonWriterInstances object is 2. Interface Syntax
 *******************************************************/
object JsonSyntax {
  // implicit class ... enrich my library pattern?
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  } }

final case class Person(name: String, email: String)

