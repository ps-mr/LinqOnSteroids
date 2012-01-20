package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree.Exp
import expressiontree.Lifting._
import optimization.Optimization
import performancetests.Benchmarking
import collection.mutable.ArrayBuffer


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

trait TransformTestHelper {
  this: JUnitSuite with ShouldMatchersForJUnit =>
  def testRebuild[T](t: Exp[T]) = {
    val tTransf = t transform identity
    tTransf should equal (t)
    assert(tTransf equals t)
    (t, tTransf)
  }
}

class TransformTest extends JUnitSuite with ShouldMatchersForJUnit with TransformTestHelper with Benchmarking {
  private val collSize = if (debugBench) 10 else 100
  private val l: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))
  private val l2: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))

  def testTransforms[T](t: Exp[T]) {
    testRebuild(t)
    testRebuild(Optimization.mergeFilters(t))
    testRebuild(Optimization.optimize(t))
  }

  @Test def testTransformIdentity {
    testTransforms(for (c <- l) yield c)
    testTransforms(for (c <- l if true) yield c)
    testTransforms(for (c <- l if c is 7) yield c)
    testTransforms(for (c <- l if (c is 7) && !(c is 19)) yield c)
    testTransforms(for (c <- l if c + 3 is 7; if c + 8 is 19) yield c)
    testTransforms(for (c <- l if (c + 3 is 7) && (c + 8 is 19)) yield c)
    testTransforms(for (k <- l; k2 <- l2 if k + k2 is k2 + k) yield k+k2)
    testTransforms(for (k <- l; k2 <- l2 if k + k2 is k2 + k) yield k*k2)
    testTransforms(for (c <- l if c * 3 is 7; if c + 8 is 19) yield c)
  }
}
