package afsalthaj

import NewType._

sealed trait Newtype[A] {
  type Type
  def apply(value: A): Type         = wrap(value)
  def unapply(value: Type): Some[A] = Some(unwrap(value))
  def wrap(value: A): Type          = wrapAll[Id](value)
  def unwrap(value: Type): A        = unwrapAll[Id](value)
  def wrapAll[F[_]](value: F[A]): F[Type]
  def unwrapAll[F[_]](value: F[Type]): F[A]
}

@annotation.nowarn("msg=differs only in case")
object NewType {

  type Id[A] = A

  def newtype[A]: Newtype[A] =
    new Newtype[A] {
      type Type = A
      def wrapAll[F[_]](value: F[A]): F[Type]   = value
      def unwrapAll[F[_]](value: F[Type]): F[A] = value
    }
}

// We will see what is subtype later.
trait Subtype[A] extends Newtype[A] {
  type Type <: A
}

@annotation.nowarn("msg=differs only in case")
object SubType {

  def subtype[A]: Subtype[A] =
    new Subtype[A] {
      type Type = A
      def wrapAll[F[_]](value: F[A]): F[Type]   = value
      def unwrapAll[F[_]](value: F[Type]): F[A] = value
    }
}

trait Equal[-A] {
  def equal(a: A, b: A): Boolean
}

object Equal {

  def apply[A](
      implicit
      ev: Equal[A]
  ): Equal[A] = ev

  implicit val equalInt: Equal[Int] = new Equal[Int] {
    def equal(a: Int, b: Int): Boolean = a == b
  }

  implicit class equalOps[A](a: A) {
    def ===(b: A)(
        implicit
        E: Equal[A]
    ) = E.equal(a, b)
  }
}

trait Monoid[A] {
  def zero: A
  def append(a: A, b: A): A
}

object Monoid {

  def apply[A](
      implicit
      ev: Monoid[A]
  ): Monoid[A] = ev

  implicit val additionInt: Monoid[Int] = new Monoid[Int] {
    def zero: Int                   = 0
    def append(a: Int, b: Int): Int = a + b
  }
}

object Example extends App {

  val Mult = NewType.newtype[Int]
  type Mult = Mult.Type

  implicit def multMonoid: Monoid[Mult] =
    new Monoid[Mult] {
      def zero: Mult                     = Mult.wrap(1)
      def append(a: Mult, b: Mult): Mult =
        Mult.wrap(Mult.unwrap(a) * Mult.unwrap(b))
    }

  // val int: Int = Mult.wrap(1) // This won't compile, and that's what we need!

  val int: Mult =
    Mult.wrap(
      1
    ) // Compiles. And we can now have a separate monoid instance of Mult that doesn't mix up with the monoid instance of Int

  Monoid[Mult].append(Mult.wrap(1), Mult.wrap(2)) == Mult.wrap(2)
  Monoid[Int].append(1, 2) == 3

  // However, now Mult is different to Int only for Monoid, and not for Equal.
  import Equal._
  1 === 1 // works
  // Mult.wrap(1) === Mult.wrap(1)  // Doesn't compile, if the below implicit doesn't exist

  // To make the above expression compile we should have an [explicit] implicit instance of Mult, which is annoying, and which is a boilerplate. Auto derivation will also require outside-mechanism.
  // Though, we have some helpers to create an instance in a concise manner. Even then, its boilerplate!
  implicit val equalMult: Equal[Mult] = Mult.wrapAll(Equal[Int])

  Mult.wrap(1) === Mult.wrap(1) // compiles

  //Ideally, we should avoid having to define an instance for Mult for every typeclass (and avoid shapless or inductive resolutions etc), as we already defined instances for Int for all of these typeclasses.

  // In scalaz, we have one way to do this which is defining {{{  implicit def equal[A: Equal, T]: Equal[A @@ T] = new Equal(....)  }}} allowing us to have Equal instance automatic for all tagged types.
  // But then, we need to write this machinary/boiler-plate written for each typeclass that we have to derive instances for tagged types.

  // Here we go one step ahead an make use of subtyping allowing us to use Mult wherever Int is required.
  // Let us look at subtype

  val Mults = SubType.subtype[Int]
  type Mults = Mults.Type

  implicit def multSubtypeMonoid: Monoid[Mults] =
    new Monoid[Mults] {
      def zero: Mults                       = Mults.wrap(1)
      def append(a: Mults, b: Mults): Mults =
        Mults.wrap(Mults.unwrap(a) * Mults.unwrap(b))
    }

  Monoid[Mults]
    .append(
      Mults.wrap(1),
      Mults.wrap(2)
    ) // returning 2 and is different to monoid instance of Int, which is addition by default.
  Monoid[Int].append(1, 2) == 3

  // Mults is different to Mult because the existential type <: A itself, allowing us to subsitutue Equal[Int] whereever Equal[Mults] is required, skipping all the boiler plates that we need

  Mults.wrap(1) === Mults.wrap(1) // compiles unlike Mult which works only with an explicit instance defined for Equal

  // The above expression works because we were able to substitute Equal[Int] in places where we need Equal[Mult]. This is because Equal[Int] is a subtype of Equal[Mult] because Mult <: Int and Equal is invariant in its types.

  // This get far better that you can now skip unwrapping Mults everytime to call a function that works on primitive Int.
  // For example, we end up having Mults in the process of using a Monoid-multipication
  val mults: Mults = Mults.wrap(1)

  def processInt(a: Int): Int = a + 1

  processInt(mults) // compiles!

  def processListOfInt(a: List[Int]): Int = a.reduce(_ + _)

  processListOfInt(
    List(mults)
  ) // Compiles, and that is because we are able to substitute List[Mults] for List[Int] because Mults <: Int, and List is covariant in its types

}
