package devinsideyou

import scala.util.chaining._
import scala.language.implicitConversions
import hutil.stringformat._
import io.estatico.newtype.macros._
import io.estatico.newtype.ops._

object Ex09NewTypeGeneric extends hutil.App {

  object UserId {

    @newtype class Opaque(val value: Int)

    object Opaque {

      def apply(value: Repr): Type =
        value.coerce

      // def unapply(userId: Opaque): Option[Int] =
      //   Option(userId).map(_.value)
    }
  }

  object ProjectId {

    @newtype class Opaque(val value: Int)

    object Opaque {

      def apply(value: Repr): Type =
        value.coerce

      // def unapply(projectId: Opaque): Option[Int] =
      //   Option(projectId).map(_.value)
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

  // "==> No extra boxing by pattern match".green.println()

  // def method3(userId: UserId.Opaque) =
  //   userId match {
  //     case UserId.Opaque(7)     => "found expected UserId 7" pipe println
  //     case UserId.Opaque(value) => s"found some UserId $value" pipe println
  //   }

  // method3(userId)
  // method3(UserId.Opaque(8))

  "==> Combining a UserId.Opaque with an Int does not compile".green.println()

  // val int1 = UserId.Opaque(8) + 5
  // val int2 = 5 + UserId.Opaque(8)
  // int1 pipe println
  // int2 pipe println
}
