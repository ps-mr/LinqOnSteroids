package ivm
package expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 28/9/2012
 */
object ExprTreesWithCSE {
  val transf: Exp[_] => Exp[_] = {
    case e => e
  }
  //Test that this code compiles:
  def tr[T](e: Exp[T]) = e transform transf

}
