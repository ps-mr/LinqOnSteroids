package ivm.expressiontree

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import Lifting._

/**
 * User: pgiarrusso
 * Date: 2/10/2012
 */
class CompileCSETest extends FunSuite with ShouldMatchers {
  Compile.reset()
  import Compile.toValueCSE
  test("basic") {
    val v1 = asExp(1) + 2
    toValueCSE(v1) should be (3)
    toValueCSE(v1 + v1) should be (6)
    toValueCSE(asExp(1) + 2 + (asExp(1) + 2)) should be (6)
  }

  test("show exponential savings") {
    val v1 = asExp(1) + 2
    def doubleNTimes(v: Exp[Int]): Int => Exp[Int] = {
      case 0 => v
      case n => doubleNTimes(v + v)(n - 1)
    }
    toValueCSE(v1) should be (3)
    //notice that this test is quite slow because the expression is fully expanded before being folded back.
    //that's damn annoying.
    toValueCSE(doubleNTimes(v1)(16)) should be (3 * 65536)
    Compile.toValue(doubleNTimes(v1)(4)) should be (3 * 16)
  }

  test("CSE respects scopes") {
    toValueCSE(letExp(1) {
      x => {
        val y = x + 1
        y + y
      }
    } + 2) should be (6)
  }

  test("CSE respects if") {
    toValueCSE(letExp(1) {
      x => {
        val y = x + 1
        if_# (x ==# 2) {y + y + 1} else_# {y + y + 2}
      }
    } + 2) should be (8)
  }
}
