package ivm
package tests

import optimization.Optimization
import expressiontree.Lifting._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import expressiontree.Exp

class NormalizationTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1, 10))

  @Test
  def testnormalization() {
    val q1 = for (c <- l if c + 3 ==# 7) yield c
    val q2 = for (c <- l if 7 ==# 3 + c) yield c
    val normalizedq1 = Optimization.normalize(q1)
    normalizedq1  should be (q2)
  }
}
