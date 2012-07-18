package performancetests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import ivm._
import expressiontree._
import Lifting._

/**
 * User: pgiarrusso
 * Date: 17/7/2012
 */

class Compilation extends FunSuite with ShouldMatchers with Benchmarking {
  Compile.reset()

  //Correctness of code generation {{{
  if (Const.allowInlineInEval) {
    test("strings") {
      asExp("foo").toCode should be ("\"foo\"")
      asExp('c').toCode should be ("'c'")
      asExp(1).toCode should be ("1")
      asExp(1: Byte).toCode should be ("(1: Byte)")
    }
  }
  test("csp") {
    asExp(List(1)).toCode should not be("List(1)")
    Compile.precompileReset()
    asExp(List(1)).toCode should be ("x1")
  }

  val e = benchMark("build expression")(((asExp(1) + 2) * 3))

  test("code generation") {
    if (Const.allowInlineInEval) {
      e.toCode should be ("((1) + (2)) * (3)")
    } else {
      e.toCode should be ("((x1) + (x2)) * (x3)")
    }
  }
  test("output") {
    if (Const.allowInlineInEval) {
      Compile.emitSource(e) should be (
        """class Outclass1() extends Compiled[Int] {
          |  override def result = ((1) + (2)) * (3)
          |}""".stripMargin)
    } else {
      Compile.emitSource(e) should be (
      """class Outclass1(val x4: Int, val x5: Int, val x6: Int) extends Compiled[Int] {
        |  override def result = ((x4) + (x5)) * (x6)
        |}""".stripMargin)
    }
  }

  class Outclass1(val x4: Int, val x5: Int, val x6: Int) extends Compiled[Int] {
    override def result = ((x4) + (x5)) * (x6)
  }

  test("benchmark and test generated code") {
    benchMark("compiled code")(new Outclass1(1, 2, 3).result) should be (9)
  }
  //}}}

  // Benchmark code generation {{{
  test("benchmark Exp.toCode") {
    benchMark("convert expression to code")(e.toCode)
  }
  test("benchmark Compile.compile") {
    benchMark("Use Compile.compile on expression")(Compile.emitSource(e))
  }
  // }}}
}
