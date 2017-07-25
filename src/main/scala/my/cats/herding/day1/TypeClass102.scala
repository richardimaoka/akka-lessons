package my.cats.herding.day1

object TypeClass102 {
  import cats._
  import cats.data._
  import cats.implicits._

  def firstAttempt(): Unit ={
    sealed trait TrafficLight
    object TrafficLight {
      case object Red extends TrafficLight
      case object Yellow extends TrafficLight
      case object Green extends TrafficLight
    }

    /**
     * A type class used to determine equality between 2 instances of the same
     * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
     * Moreover, `eqv` should form an equivalence relation.

     trait Eq[@sp A] extends Any with Serializable { self =>
        /**
     * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
     */
        def eqv(x: A, y: A): Boolean

        /**
     * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
     */
        def neqv(x: A, y: A): Boolean = !eqv(x, y)
     *
     */
    implicit val trafficLightEq: Eq[TrafficLight] =
      new Eq[TrafficLight] {
        /**
         * Instance of Eq[] needs to implement eqv
         */
        def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
      }

    /**
     *  TypeClass102.scala:41: value === is not a member of object
     *  my.cats.herding.day1.TypeClass102.TrafficLight.Red
     *   [error]     TrafficLight.Red === TrafficLight.Yellow
     *
     */
    //TrafficLight.Red === TrafficLight.Yellow

  }

  def secondAttempt(): Unit ={
    sealed trait TrafficLight
    object TrafficLight {
      def red: TrafficLight = Red
      def yellow: TrafficLight = Yellow
      def green: TrafficLight = Green
      case object Red extends TrafficLight
      case object Yellow extends TrafficLight
      case object Green extends TrafficLight
    }

    implicit val trafficLightEq: Eq[TrafficLight] =
      new Eq[TrafficLight] {
        /**
         * Instance of Eq[] needs to implement eqv
         */
        def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
      }

    //wazzup ?! this also fails to compile with the same error
    //TrafficLight.Red === TrafficLight.Yellow
    /**
     *  TypeClass102.scala:71: value === is not a member of object
     *  my.cats.herding.day1.TypeClass102.TrafficLight.Red
     *   [error]     TrafficLight.Red === TrafficLight.Yellow
     *
     */
  }


  def main(args: Array[String]): Unit ={
  }
}
