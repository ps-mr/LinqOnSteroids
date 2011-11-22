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
import performancetests.Benchmarking.benchMark
import collection.mutable.ArrayBuffer
import tests.TransformTestHelper


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class TransformedFuncPerfTests extends JUnitSuite with ShouldMatchersForJUnit with TransformTestHelper {
  val debug = false
  val warmUpLoops = if (debug) 1 else 4000
  val sampleLoops = if (debug) 2 else 50
  val execLoops = 10
  val collSize = if (debug) 10 else 100
  val l: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))
  val l2: Exp[Traversable[Int]] = toExp(ArrayBuffer.range(1, collSize))

  def testBenchmarkTransforms[T](t: Exp[T]) {
    val (_, tTransf) = testRebuild(t)
    benchMark("base #1", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      t.interpret()
    }
    benchMark("base #2", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      t.interpret()
    }
    benchMark("after transform identity #1", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tTransf.interpret()
    }
    benchMark("after transform identity #2", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tTransf.interpret()
    }
    benchMark("after transform identity #3", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tTransf.interpret()
    }
    val tOptim = Optimization.optimize(t)
    benchMark("after optimization #1", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tOptim.interpret()
    }
    benchMark("after optimization #2", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops, execLoops = execLoops) {
      tOptim.interpret()
    }
    println(t)
    println(tTransf)
    println(tOptim)
  }

  @Test def testTransformIdentityMap1 {
    val query = for (c <- l) yield c + 1
    testBenchmarkTransforms(query)
  }
}
