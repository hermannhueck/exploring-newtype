package francistoth

import scala.util.chaining._
import hutil.stringformat._

object Step00AmbiguousImplicits extends hutil.App {

  s"$dash10 compile error: ambiguous implicit values if more than one Associative[Int] is in scope".magenta.println()

  implicit val intSumAssociative: Associative[Int] = _ + _
  // implicit val intProductAssociative: Associative[Int] = _ * _

  /*
    compile error: ambiguous implicit values
    if more than one Associative[Int] is in scope
   */
  Associative.reduce(1, List(2, 3)) pipe println // 5
}
