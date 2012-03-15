package ivm
package tests

import optimization.Optimization
import expressiontree.Lifting._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import expressiontree.Exp

class MergeFilterTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1, 10))

  @Test
  def testMerging() {
    val q1 = for (c <- l if c + 3 ==# 7; if c + 8 ==# 19 ) yield c
    val mergedq1 = Optimization.mergeFilters(q1)
    val desiredResult = for (c <- l if (c + 3 ==# 7) && (c + 8 ==# 19)) yield c
    mergedq1 should equal (desiredResult)
  }
}
