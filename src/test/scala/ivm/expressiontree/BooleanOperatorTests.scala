package ivm.expressiontree

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.Test
import Lifting._

class BooleanOperatorTests extends JUnitSuite with Matchers with AssertionsForJUnit {
  def expSet[A](elems: Exp[A]*): Set[Exp[A]] = Set(elems: _*)
  @Test
  def testcnfconversion() {
    val o: Exp[Boolean] = true
    val res = BooleanOperators.cnf(o)
    res should  be (expSet(Const(true)))

    val o2 = Or(Var(0), Var(1))
    val r2 = BooleanOperators.cnf(o2)
    r2 should be (expSet(Or(Var(0),Var(1))))


    val o3: Exp[Boolean] = Not(Or(Not(And(Var(0), Not(And(Var(1),Var(2))))), Not(Var(0))))
    val r3 = BooleanOperators.cnf(o3)
    r3 should be (expSet(Var(0), Or(Not(Var(1)),Not(Var(2)))))

    val o4: Exp[Boolean] = Or(And(Var(0), Var(1)), And(Var(2), Var(3)))
    val r4 = BooleanOperators.cnf(o4)
    r4 should be (expSet(Or(Var(0), Var(2)), Or(Var(0), Var(3)), Or(Var(1), Var(2)), Or(Var(1), Var(3))))
  }

  @Test
  def testShortCircuit() {
    val coll = Seq(None, Some(1)).asSquopt
    val query = for {
      v <- coll
      //This line executes correctly only if And is short-circuiting, as it should be.
      if v.isDefined && v.get ==# 1
    } yield v.get
    query.value() should be (Traversable(1))
  }
}
