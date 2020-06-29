/*
  See video on newtype by DevInsideYou:
  https://www.youtube.com/watch?v=WyvawRRuU2c
 */
package newtype.devinsideyou

import scala.util.chaining._
import hutil.stringformat._

object Ex05TaggedType extends hutil.App {

  object UserId {

    // final case class Opaque(value: Int) extends AnyVal

    trait Tag
    type Opaque = Int with Tag

    object Opaque {

      def apply(value: Int): Opaque =
        value.asInstanceOf[Opaque]

      def unapply(userId: Opaque): Option[Int] =
        Option(userId).map(_.value)
    }

    implicit final class Ops(private val userId: Opaque) extends AnyVal {
      def value: Int = userId.asInstanceOf[Int]
    }
  }

  object ProjectId {

    // final case class Opaque(value: Int) extends AnyVal

    trait Tag
    type Opaque = Int with Tag

    object Opaque {

      def apply(value: Int): Opaque =
        value.asInstanceOf[Opaque]

      def unapply(projectId: Opaque): Option[Int] =
        Option(projectId).map(_.value)
    }

    implicit final class Ops(private val projectId: Opaque) extends AnyVal {
      def value: Int = projectId.asInstanceOf[Int]
    }
  }

  def myMethod(userId: UserId.Opaque, projectId: ProjectId.Opaque): Unit =
    s"UserId = ${userId.value}, ProjectId = ${projectId.value}" pipe println

  val userId    = UserId.Opaque(7)
  val projectId = ProjectId.Opaque(13)

  myMethod(userId, projectId)
  "==> Extending AnyVal avoids boxing (but only in trival cases)".green.println()
  // myMethod(projectId, userId) // does not compile

  "==> Functional overloading doesn't work due to type erasure".red.println()

  def method2(userId: UserId.Opaque) = println(userId)
  // def method2(projectId: ProjectId.Opaque) = println(projectId) // does not compile
  method2(userId)

  "==> No extra boxing by pattern match".green.println()

  def method3(userId: UserId.Opaque) =
    userId match {
      case UserId.Opaque(7)     => "found expected UserId 7" pipe println
      case UserId.Opaque(value) => s"found some UserId $value" pipe println
    }

  method3(userId)
  method3(UserId.Opaque(8))

  "==> Int is leaking out when combining a UserId.Opaque with an Int; should not compile".red.println()

  val int1 = UserId.Opaque(8) + 5
  val int2 = 5 + UserId.Opaque(8)
  int1 pipe println
  int2 pipe println
}
