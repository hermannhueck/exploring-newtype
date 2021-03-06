package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step07IntermediateClass extends hutil.App {

  s"$dash10 Use intermediate sub class".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  sealed trait Newtype[A] {
    type WrappedType
    def apply(a: A): WrappedType
    def unwrap(wt: WrappedType): A
  }

  object Newtype {
    // create an object extending this Subtype
    abstract class Subtype[A] extends Newtype[A] {
      override type WrappedType <: A
      override def apply(a: A): WrappedType   = a.asInstanceOf[WrappedType]
      override def unwrap(wt: WrappedType): A = wt
    }
  }

  object Mult extends Newtype.Subtype[Int]
  type Mult = Mult.WrappedType

  val m0: Mult = Mult(1) // compiles
  // val m1: Int  = Mult(1) // does not compile: a Mult.WrappedType is not an Int
  val i0: Int  = Mult.unwrap(m0)

  implicit val multProductAssociative: Associative[Mult] =
    (a0, a1) => (Mult.unwrap(a0) * Mult.unwrap(a1)) pipe Mult.apply

  val ints  = List(3, 5)
  val mults = ints.map(Mult.apply)

  Associative.reduce(0, ints) pipe println        // 8
  Associative.reduce(Mult(1), mults) pipe println // 15

  /*
    Pattern matching doesn't work. Requires unapply.
   */
  // def meaningOfLife(mult: Mult): Unit =
  //   mult match {
  //     case Mult(meaningOfLife) => s"Found the meaning of life: $meaningOfLife!" pipe println
  //     case Mult(noMeaning)     => s"Life has no meaning: $noMeaning" pipe println
  //   }

  // meaningOfLife(Mult(42))
}
