package ivm.expressiontree

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import Lifting._

/**
 * User: pgiarrusso
 * Date: 2/10/2012
 */
class CompileCSETest extends FunSuite with ShouldMatchers {
  import Compile.{reset, toValueCSE}
  test("basic") {
    reset()
    val v1 = asExp(1) + 2
    toValueCSE(v1) should be (3)
    toValueCSE(v1 + v1) should be (6)
    toValueCSE(asExp(1) + 2 + (asExp(1) + 2)) should be (6)
  }

  test("show exponential savings") {
    reset()
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
    reset()
    toValueCSE(letExp(1) {
      x => {
        val y = x + 1
        y + y
      }
    } + 2) should be (6)
  }

  test("CSE respects if") {
    reset()
    toValueCSE(letExp(1) {
      x => {
        val y = x + 1
        if_# (x ==# 2) {y + y + 1} else_# {y + y + 2}
      }
    } + 2) should be (8)
  }

  test("CSE works on for comprehensions") {
    reset()
    toValueCSE(for {
      i <- (1 to 10).asSquopt
      if i % 2 ==# 0
    } yield i) should be (2 to (10, step = 2))
  }

  test("CSE works on persistent nodes") {
    reset()
    toValueCSE(asExp(1).ifInstanceOf[Int]) should be (Some(1))
  }

  test("CSE does not move non-terminating expressions out of functions") {
    reset()
    //val empty = Seq.empty.asSquopt //too hard for Scalac sometimes.
    val empty = Seq.empty[Int].asSquopt
    toValueCSE(empty.filter(x => empty.head != null)) should be (Seq.empty)
    toValueCSE(empty.map(x => empty.head)) should be (Seq.empty)
  }

  test("CSE does not move non-terminating expressions out of ifs") {
    reset()
    val empty = Seq.empty.asSquopt
    toValueCSE(if_# (asExp(1) % 2 ==# 0) {empty.head} else_# {0}) should be (0)
  }
}
