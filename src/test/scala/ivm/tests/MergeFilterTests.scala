package ivm
package tests

import optimization.Optimization
import collections.CollectionReifier
import expressiontree.Lifting._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class MergeFilterTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: CollectionReifier[Int] = new CollectionReifier(Vector.range(1, 10))

  @Test
  def testMerging() {
    val q1 = for (c <- l if c + 3 is 7; if c + 8 is 19 ) yield c
    val mergedq1 = Optimization.mergeFilters(q1)
    val desiredResult = for (c <- l if (c + 3 is 7) && (c + 8 is 19)) yield c
    assert(mergedq1.potentiallyEquals(desiredResult))
  }
}
