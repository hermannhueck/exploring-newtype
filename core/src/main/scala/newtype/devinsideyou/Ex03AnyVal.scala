/*
  See video on newtype by DevInsideYou:
  https://www.youtube.com/watch?v=WyvawRRuU2c
 */
package newtype.devinsideyou

import scala.util.chaining._
import hutil.stringformat._

object Ex03AnyVal extends hutil.App {

  final case class UserId(value: Int)    extends AnyVal
  final case class ProjectId(value: Int) extends AnyVal

  def myMethod(userId: UserId, projectId: ProjectId): Unit =
    s"UserId = ${userId.value}, ProjectId = ${projectId.value}" pipe println

  val userId    = UserId(7)
  val projectId = ProjectId(13)

  myMethod(userId, projectId)
  "==> Extending AnyVal avoids boxing (but only in trival cases)".green.println()
  // myMethod(projectId, userId) // does not compile

  "==> Functional overloading doesn't work due to type erasure".red.println()

  def method2(userId: UserId) = println(userId)
  // def method2(projectId: ProjectId) = println(projectId) // does not compile
  method2(userId)

  "==> Pattern match leads to value boxing".red.println()

  def method3(userId: UserId) =
    userId match {
      case UserId(7)     => "found expected UserId 7" pipe println
      case UserId(value) => s"found some UserId $value" pipe println
    }

  method3(userId)
  method3(UserId(8))
}
