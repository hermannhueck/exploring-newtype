/*
  See video on newtype by DevInsideYou:
  https://www.youtube.com/watch?v=WyvawRRuU2c
 */
package newtype.devinsideyou

import scala.util.chaining._
import hutil.stringformat._

object Ex04ScopedCaseClass extends hutil.App {

  object UserId    {
    final case class Opaque(value: Int) extends AnyVal
  }
  object ProjectId {
    final case class Opaque(value: Int) extends AnyVal
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

  "==> Pattern match leads to value boxing".red.println()

  def method3(userId: UserId.Opaque) =
    userId match {
      case UserId.Opaque(7)     => "found expected UserId 7" pipe println
      case UserId.Opaque(value) => s"found some UserId $value" pipe println
    }

  method3(userId)
  method3(UserId.Opaque(8))
}
