package ivm.tests


import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import ivm.collections.CollectionReifier
import ivm.optimization.Optimization
import org.scalatest.Assertions.assert
import ivm.expressiontree.Lifting._

class GroupByTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: CollectionReifier[Int] = new CollectionReifier(Vector.range(1, 10))

  @Test
  def testGroupBy() {
    val q1 = l.groupBy( (x) => x <= 5)(true)
    q1.exec() should be (Vector(1,2,3,4,5))
  }
}