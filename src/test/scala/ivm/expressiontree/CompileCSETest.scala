package ivm.expressiontree

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import Lifting._

/**
 * User: pgiarrusso
 * Date: 2/10/2012
 */
class CompileCSETest extends FunSuite with ShouldMatchers {
  test("basic") {
    val v1 = asExp(1) + 2
    Compile.toValueCSE(v1) should be (3)
    Compile.toValueCSE(v1 + v1) should be (6)
    Compile.toValueCSE(asExp(1) + 2 + (asExp(1) + 2)) should be (6)
  }

  test("show exponential savings") {
    val v1 = asExp(1) + 2
    def doubleNTimes(v: Exp[Int]): Int => Exp[Int] = {
      case 0 => v
      case n => doubleNTimes(v + v)(n - 1)
    }
    Compile.toValueCSE(v1) should be (3)
    //notice that this test is quite slow because the expression is fully expanded before being folded back.
    //that's damn annoying.
    Compile.toValueCSE(doubleNTimes(v1)(16)) should be (3 * 65536)
    Compile.toValue(doubleNTimes(v1)(4)) should be (3 * 16)
  }
}
