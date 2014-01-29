package ivm.tests


import org.scalatest.junit.JUnitSuite
import org.scalatest.Matchers
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import ivm.expressiontree.Exp
import ivm.optimization.Optimization
import ivm.expressiontree.Lifting._

class GroupByTests extends JUnitSuite with Matchers with AssertionsForJUnit {
  val l: Exp[Traversable[Int]] = pure(Vector.range(1, 10))

  @Test
  def testGroupBy() {
    val q = l.indexBy(_ <= 5)
    val q1 = q(true)
    q1.eval should be (Vector(1,2,3,4,5))
  }
}
