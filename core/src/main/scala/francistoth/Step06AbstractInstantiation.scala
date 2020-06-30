package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step06AbstractInstantiation extends hutil.App {

  s"$dash10 Abstract instantiation of Newtype".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  sealed trait Newtype[A] {
    type WrappedType
    def apply(a: A): WrappedType
    def unwrap(wt: WrappedType): A
  }

  object Newtype {
    def instance[A]: Newtype[A] =
      new Newtype[A] {
        override type WrappedType = A
        override def apply(a: A): WrappedType   = a
        override def unwrap(wt: WrappedType): A = wt
      }
  }

  val Mult: Newtype[Int] = Newtype.instance[Int]
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
}
