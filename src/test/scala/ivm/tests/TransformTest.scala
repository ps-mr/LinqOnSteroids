package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree.Exp
import expressiontree.Lifting._
import optimization.Optimization


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class TransformTest extends JUnitSuite with ShouldMatchersForJUnit {
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1, 10))
  val l2: Exp[Traversable[Int]] = toExp(Vector.range(1, 10))

  def testRebuild[T](t: Exp[T]) {
    val tTransf = t transform identity
    tTransf should equal (t)
    assert(tTransf equals t)
  }
  def testTransforms[T](t: Exp[T]) {
    testRebuild(t)
    testRebuild(Optimization.mergeFilters(t))
  }
  
  @Test def testTransformIdentity {
    testTransforms(for (c <- l) yield c)
    testTransforms(for (c <- l if true) yield c)
    testTransforms(for (c <- l if c is 7) yield c)
    testTransforms(for (c <- l if (c is 7) && !(c is 19)) yield c)
    testTransforms(for (c <- l if c + 3 is 7; if c + 8 is 19) yield c)
    testTransforms(for (c <- l if (c + 3 is 7) && (c + 8 is 19)) yield c)
    testTransforms(for (k <- l; k2 <- l2 if k + k2 is k2 + k) yield k+k2)
  }
}
