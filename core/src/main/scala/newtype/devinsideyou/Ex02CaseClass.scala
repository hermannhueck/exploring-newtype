/*
  See video on newtype by DevInsideYou:
  https://www.youtube.com/watch?v=WyvawRRuU2c
 */
package newtype.devinsideyou

import scala.util.chaining._
import hutil.stringformat._

object Ex02CaseClass extends hutil.App {

  final case class UserId(value: Int)
  final case class ProjectId(value: Int)

  def myMethod(userId: UserId, projectId: ProjectId): Unit =
    s"UserId = ${userId.value}, ProjectId = ${projectId.value}" pipe println

  val userId    = UserId(7)
  val projectId = ProjectId(13)

  myMethod(userId, projectId)
  "==> Params cannot be exchanged, code does not compile!".green.println()
  // myMethod(projectId, userId) // does not compile
}
