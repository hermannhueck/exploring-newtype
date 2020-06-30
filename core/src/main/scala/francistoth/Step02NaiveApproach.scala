package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step02NaiveApproach extends hutil.App {

  s"$dash10 Naive Approach: A Mult.WrappedType is just a type alias for Int".magenta.println()
  s"$dash10 Mult.WrappedType and Int are not different for the compiler".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  sealed trait Newtype[A] {
    type WrappedType = A
    def wrap(a: A): WrappedType = a
  }

  object Mult extends Newtype[Int]
  type Mult = Mult.WrappedType

  val m0: Mult = Mult.wrap(1) // compiles
  val m1: Int  = Mult.wrap(1) // compiles, but should not

  // implicit val intProductAssociative: Associative[Mult] =
  //   (a0, a1) => a0 * a1

  val ints  = List(3, 5)
  val mults = ints.map(Mult.wrap)

  /*
    compile error: ambiguous implicit values
    if more than one Associative[Int] is in scope
   */
  Associative.reduce(0, ints) pipe println             // 8
  Associative.reduce(Mult.wrap(1), mults) pipe println // 9 -- should be 15
}
