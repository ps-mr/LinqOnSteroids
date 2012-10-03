package ivm
package expressiontree

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import Lifting._
import performancetests.Benchmarking

/**
 * User: pgiarrusso
 * Date: 17/7/2012
 */

class Compilation extends FunSuite with ShouldMatchers with Benchmarking {
  Compile.reset()

  import Compile.toCode

  //Correctness of code generation {{{
  if (Const.allowInlineInEval) {
    test("strings") {
      toCode(asExp("foo")) should be("\"foo\"")
      toCode(asExp('c')) should be("'c'")
      toCode(asExp(1)) should be("1")
      toCode(asExp(1: Byte)) should be("(1: Byte)")
    }
  }
  test("csp") {
    toCode(asExp(List(1))) should not be ("List(1)")
    Compile.precompileReset()
    toCode(asExp(List(1))) should be("x1")
  }

  val e = benchMark("build expression")(((asExp(1) + 2) * 3))

  test("code generation") {
    if (Const.allowInlineInEval) {
      toCode(e) should be("((1) + (2)) * (3)")
    } else {
      Compile.precompileReset()
      toCode(e) should be("((x1) + (x2)) * (x3)")
    }
    Compile.precompileReset()
    toCode(expOption2Iterable(Some(1))) should be("Option.option2Iterable(x1)")
    toCode(expOption2Iterable(Some(1)).transform(identity)) should be("Option.option2Iterable(x1)")
  }
  test("output") {
    if (Const.allowInlineInEval) {
      Compile.emitSource(e) should be(
        """class Outclass1() extends ivm.expressiontree.Compiled[Int] {
          |  override def result = ((1) + (2)) * (3)
          |}""".stripMargin)
    } else {
      Compile.emitSource(e) should be(
        """class Outclass1(val x1: Int, val x2: Int, val x3: Int) extends ivm.expressiontree.Compiled[Int] {
          |  override def result = ((x1) + (x2)) * (x3)
          |}""".stripMargin)
    }
  }

  class Outclass1(val x1: Int, val x2: Int, val x3: Int) extends Compiled[Int] {
    override def result = ((x1) + (x2)) * (x3)
  }

  test("benchmark and test generated code") {
    benchMark("compiled code")(new Outclass1(1, 2, 3).result) should be(9)
  }
  //}}}

  // Benchmark code generation {{{
  test("benchmark Exp.toCode") {
    benchMark("convert expression to code")(toCode(e))
  }
  test("benchmark Compile.emitSource") {
    benchMark("Use Compile.emitSource on expression")(Compile.emitSource(e))
  }
  test("benchMark Compile.toValue") {
    benchMark("Use Compile.toValue on expression")(Compile.toValue(e)) should be(9)
  }
  test("benchMark Compile.toValue for CSP") {
    benchMark("Use Compile.toValue on CSP expression")(Compile.toValue(asExp(List(1)))) should be(List(1))
  }
  test("benchMark Compile.toValue for collection expression") {
    benchMark("Use Compile.toValue on collection expression")(Compile.toValue(asExp(List(1, 2, 3)) map (_ + 1))) should be(List(2, 3, 4))
  }
  // }}}
}
