package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step04UsingSubtype extends hutil.App {

  s"$dash10 Requiring: type WrappedType <: A".magenta.println()
  s"$dash10 This lets us again treat Mult and Mult.WrappedType as Int and avoids unwrapping".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  sealed trait Subtype[A] {
    // require that WrappedType is a subtype of A
    type WrappedType <: A
    def wrap(a: A): WrappedType
    // we don't need an unwrap method b/c WrappedType can be treated as A
  }

  val Mult: Subtype[Int] = new Subtype[Int] {
    type WrappedType = Int
    override def wrap(a: Int): WrappedType = a
  }
  type Mult = Mult.WrappedType

  val m0: Mult = Mult.wrap(1) // compiles
  val m1: Int  = Mult.wrap(1) // !!! this compiles again

  implicit val multProductAssociative: Associative[Mult] =
    (a0, a1) => (a0 * a1) pipe Mult.wrap

  val ints  = List(3, 5)
  val mults = ints.map(Mult.wrap)

  Associative.reduce(0, ints) pipe println             // 8
  Associative.reduce(Mult.wrap(1), mults) pipe println // 15
}
