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
  val e = benchMark("build expression")(((asExp(1) + 2) * 3))
  test("code generation") {
    benchMark("convert expression to code")(e.toCode)
  }
}
