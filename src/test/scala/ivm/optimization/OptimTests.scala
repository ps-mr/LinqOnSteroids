package ivm
package optimization

import expressiontree.Lifting._
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree.{Const, Plus, FuncExp, Exp}

/**
 * User: pgiarrusso
 * Date: 2/1/2012
 */

class OptimTests extends JUnitSuite with ShouldMatchersForJUnit {
  @Test
  def reassociateOps() {
    def f(e: Exp[Int]) = e + 1
    val composedF = FuncExp((x: Exp[Int]) => f(f(f(x))))

    Optimization.reassociateOps(composedF) match {
      case f@FuncExpBody(Plus(Const(3), x)) if f.x == x =>
        true
      case _ => false
    }
  }
}
