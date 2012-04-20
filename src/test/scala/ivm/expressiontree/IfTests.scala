package ivm
package expressiontree

import Lifting._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class IfTests extends FunSuite with ShouldMatchers {
  test("if-1") {
    (if_# (asExp(1) + 2 ==# 4) {1} else_# if_# (asExp(1) + 2 ==# 3) {2} else_# 3).expResult() should be (2)
  }

  test("if should work in a function body") {
    val f: Fun[Int, Int] = Fun(x => (if_# (x % 3 ==# 0) {0} else_# if_# (x % 3 ==# 1) {1} else_# 2))
    for (i <- 0 to 2)
     f(i).expResult() should be (i)
  }
}
