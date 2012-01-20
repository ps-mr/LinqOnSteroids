package ivm
package performancetests

/**
 * User: pgiarrusso
 * Date: 22/11/2011
 */

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree.Exp
import expressiontree.Lifting._
import optimization.Optimization
import collection.mutable.ArrayBuffer
import tests.TransformTestHelper


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class TransformedFuncPerfTests extends JUnitSuite with ShouldMatchersForJUnit with TransformTestHelper with Benchmarking {
  override val warmUpLoops = 4000
  override val effectiveExecLoops = 10
  val collSize = if (debugBench) 10 else 100
  val l: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))
  val l2: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))

  def testBenchmarkTransforms[T](t: Exp[T]) {
    val (_, tTransf) = testRebuild(t)
    println(t)
    benchMark("base #1") {
      t.interpret()
    }
    benchMark("base #2") {
      t.interpret()
    }

    println(tTransf)
    benchMark("after transform identity #1") {
      tTransf.interpret()
    }
    benchMark("after transform identity #2") {
      tTransf.interpret()
    }
    benchMark("after transform identity #3") {
      tTransf.interpret()
    }

    val tOptim = Optimization.optimize(t)
    println(tOptim)
    benchMark("after optimization #1") {
      tOptim.interpret()
    }
    benchMark("after optimization #2") {
      tOptim.interpret()
    }
  }

  @Test def testTransformIdentityMap1 {
    val query = for (c <- l) yield c + 1
    testBenchmarkTransforms(query)
  }
}
