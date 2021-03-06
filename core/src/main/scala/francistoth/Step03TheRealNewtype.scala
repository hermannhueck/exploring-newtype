package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step03TheRealNewtype extends hutil.App {

  s"$dash10 Breaking the connection between Mult.WrappedType and Int by subtyping".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _

  sealed trait Newtype[A] {
    type WrappedType
    def wrap(a: A): WrappedType
    def unwrap(a: WrappedType): A
  }

  val Mult: Newtype[Int] = new Newtype[Int] {
    override type WrappedType = Int
    override def wrap(a: Int): WrappedType   = a
    override def unwrap(a: WrappedType): Int = a
  }
  type Mult = Mult.WrappedType

  val m0: Mult = Mult.wrap(1) // compiles
  // val m1: Int  = Mult.wrap(1) // does not compile: a Mult.WrappedType is not an Int
  val i0: Int  = Mult.unwrap(m0)

  implicit val multProductAssociative: Associative[Mult] =
    (a0, a1) => (Mult.unwrap(a0) * Mult.unwrap(a1)) pipe Mult.wrap

  val ints  = List(3, 5)
  val mults = ints.map(Mult.wrap)

  Associative.reduce(0, ints) pipe println             // 8
  Associative.reduce(Mult.wrap(1), mults) pipe println // 15
}
