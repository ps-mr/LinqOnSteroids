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
  val x = FuncExp.gensym[Int]()

  def testIdempotence[T](e: Exp[T]) = {
    val opt = Optimization.reassociateOps(e)
    opt should be (Optimization.reassociateOps(opt))
    opt
  }
  
  @Test
  def reassociateOpsF() {
    def f(e: Exp[Int]) = e + 1
    val composedF = f(f(f(x)))

    val optF = testIdempotence(composedF)
    optF should be (Plus(Const(3), x))
  }

  @Test
  def reassociateOpsG() {
    def g(e: Exp[Int]) = 1 + e
    val composedG = g(g(g(x)))
    val optG = testIdempotence(composedG)
    optG should be (Plus(Const(3), x))
  }

  @Test
  def reassociateOpsH() {
    val h = 1 + (1 + x) + 1
    val optH = testIdempotence(h)
    optH should be (Plus(Const(3), x))
  }

  //Optimization results below are not the best, but it's hard to get this kind of patterns right in general
  //(consider increasing distance between constants)
  @Test
  def reassociateOpsI() {
    val i = x + x + 1 + x + 1
    val optI = testIdempotence(i)
    optI should be (Plus(Plus(Plus(Const(2), x), x), x))
  }

  @Test
  def reassociateOpsJ() {
    val j = x + 1 + x + 1
    val optJ = testIdempotence(j)
    optJ should be (Plus(Plus(Const(2), x), x))
  }
}
