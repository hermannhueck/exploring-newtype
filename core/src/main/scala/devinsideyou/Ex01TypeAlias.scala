package devinsideyou

import scala.util.chaining._
import hutil.stringformat._

object Ex01TypeAlias extends hutil.App {

  type UserId    = Int
  type ProjectId = Int

  def myMethod(userId: UserId, projectId: ProjectId): Unit =
    s"UserId = $userId, ProjectId = $projectId" pipe println

  val userId    = 7
  val projectId = 13

  myMethod(userId, projectId)
  "==> Params can be exchanged, code should not compile!".red.println()
  myMethod(projectId, userId)
}
