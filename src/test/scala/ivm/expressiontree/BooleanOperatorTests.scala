package ivm.expressiontree

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import Lifting._

class BooleanOperatorTests extends JUnitSuite with ShouldMatchersForJUnit {
  @Test
  def testcnfconversion() {
    val o : Exp[Boolean] = true
    val res = BooleanOperators.cnf(o)
    res should  be (Set(Const(true)))

    val o2 = Or(Var("x"), Var("y"))
    val r2 = BooleanOperators.cnf(o2)
    r2 should be (Set(Or(Var("x"),Var("y"))))


    val o3 : Exp[Boolean] = Not(Or(Not(And(Var("x"), Not(And(Var("y"),Var("z"))))), Not(Var("x"))))
    val r3 = BooleanOperators.cnf(o3)
    println(r3)
    //res should be (Set(Or(Var("x",Var("y")))))
  }

}
