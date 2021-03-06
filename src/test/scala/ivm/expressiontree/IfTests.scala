package ivm
package expressiontree

import Lifting._
import org.scalatest.FunSuite
import org.scalatest.Matchers

class IfTests extends FunSuite with Matchers {
  test("if-1") {
    (if_# (asExp(1) + 2 ==# 4) {1} else_# if_# (asExp(1) + 2 ==# 3) {2} else_# 3).value() should be (2)
  }

  test("if should work in a function body") {
    val f: Exp[Int => Int] = Sym(Fun(x => (if_# (x % 3 ==# 0) {0} else_# if_# (x % 3 ==# 1) {1} else_# 2)))
    for (i <- 0 to 2)
     f(i).value() should be (i)
  }
}
