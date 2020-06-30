package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step01ValueType extends hutil.App {

  s"$dash10 Value Type".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  case class Mult(value: Int) extends AnyVal
  implicit val intProductAssociative: Associative[Mult] =
    (a0, a1) => Mult(a0.value * a1.value)

  val ints  = List(2, 3)
  val mults = ints.map(Mult)

  Associative.reduce(1, ints) pipe println        // 5
  Associative.reduce(Mult(1), mults) pipe println // Mult(6)
}
