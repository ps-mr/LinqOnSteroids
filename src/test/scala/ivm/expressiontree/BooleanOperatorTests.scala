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

    val o2 = Or(Var(0), Var(1))
    val r2 = BooleanOperators.cnf(o2)
    r2 should be (Set(Or(Var(0),Var(1))))


    val o3 : Exp[Boolean] = Not(Or(Not(And(Var(0), Not(And(Var(1),Var(2))))), Not(Var(0))))
    val r3 = BooleanOperators.cnf(o3)
    r3 should be (Set(Var(0), Or(Not(Var(1)),Not(Var(2)))))
  }

}
