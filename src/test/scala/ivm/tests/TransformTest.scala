package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree.Exp
import expressiontree.Lifting._
import optimization.Optimization
import performancetests.Benchmarking.benchMark


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class TransformTest extends JUnitSuite with ShouldMatchersForJUnit {
  val debug = false
  val warmUpLoops = if (debug) 1 else 2000
  val sampleLoops = if (debug) 2 else 50
  val execLoops = 10
  val collSize = if (debug) 10 else 100
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1, collSize))
  val l2: Exp[Traversable[Int]] = toExp(Vector.range(1, collSize))

  def testRebuild[T](t: Exp[T]) = {
    val tTransf = t transform identity
    tTransf should equal (t)
    assert(tTransf equals t)
    (t, tTransf)
  }

  def testTransforms[T](t: Exp[T]) {
    testRebuild(t)
    testRebuild(Optimization.mergeFilters(t))
    testRebuild(Optimization.optimize(t))
  }

  def testBenchmarkTransforms[T](t: Exp[T]) {
    val (_, tTransf) = testRebuild(t)
    benchMark("base", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      t.interpret()
    }
    benchMark("base again", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      t.interpret()
    }
    benchMark("after transform identity", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tTransf.interpret()
    }
    benchMark("after transform identity, again", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tTransf.interpret()
    }
    val tOptim = Optimization.optimize(t)
    benchMark("after optimization", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tOptim.interpret()
    }
    benchMark("after optimization, again", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tOptim.interpret()
    }
    println(t)
    println(tTransf)
    println(tOptim)
  }

  @Test def testTransformIdentityMap1 {
    val query = for (c <- l) yield c + 1
    testBenchmarkTransforms(query)
    println()
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
