package ivm.expressiontree

import org.scalatest.Matchers
import org.scalatest.FunSuite
import Lifting._

/**
 * User: pgiarrusso
 * Date: 26/1/2012
 */

class FuncExpTests extends FunSuite with Matchers {
  val f = Fun((x: Exp[Int]) => Fun((y: Exp[Int]) => x + y))
  val f1 = f.interpret()(1) //(x = 1, y => x + y)

  val expr = (x: Exp[Int]) => asExp(f1)
  val f3 = asExp(new FunInterp(expr(f.x), f.x)) //Under dynamic scoping, this will just fail

  test("scoping 1") {
    f3.interpret()(10)(2) should be (3)
  }
  test("scoping 2") {
    f3(10)(2).interpret() should be (3)
  }

  val f2 = asExp(Fun((z: Exp[Int]) => f1)) //Under dynamic scoping, this will just fail.

  test("scoping 3") {
    f2(10)(2).interpret() should be (3)
  }
  test("scoping 4") {
    f2.interpret()(10)(2) should be (3)
  }

  val f4 = asExp(new FunInterp(expr(f.x)(2), f.x)) //Under dynamic scoping, the new binding for x in expr will be used within f1 in scoping 5.
  test("scoping 5") {
    f4.interpret()(10) should be (3)
  }
  test("scoping 6") {
    f4(10).interpret() should be (3)
  }
}
