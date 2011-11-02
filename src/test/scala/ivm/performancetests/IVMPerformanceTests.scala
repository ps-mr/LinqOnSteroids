package ivm
package performancetests

import org.scalatest.junit.{JUnitSuite, ShouldMatchersForJUnit}
import org.junit.Test
import collection.mutable

import performancetests.Benchmarking._
import tests.IVMTestUtil
import expressiontree._
import Lifting._
import collections.IncHashSet

/**
 * User: pgiarrusso
 * Date: 2/11/2011
 */

class IVMPerformanceTests extends JUnitSuite with ShouldMatchersForJUnit with IVMTestUtil {
  val warmUpLoops = 100
  val sampleLoops = 50

  @Test
  def aNative() {
    val v = new mutable.HashSet[Int]
    var res: mutable.HashSet[Int] = null
    benchMark("nativeMap", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= 1 until 10 * 1000
      res = v map (_ + 1)
    }
  }

  def testFillAndUpdate(title: String, v: IncHashSet[Int]) {
    benchMark(title, warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= 1 until 10 * 1000
    }
    benchMark(title + " - upd", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v += 1
    }
  }

  @Test def incHashSet() {
    val v = new IncHashSet[Int]
    testFillAndUpdate("IncHashSet", v)
  }

  @Test def incHashSetAndIncRes() {
    val v = new IncHashSet[Int]
    val incrementalResult = new IncrementalResult[Int](v)
    testFillAndUpdate("IncHashSet & IncRes", v)
    incrementalResult.interpret() should be (incrementalResult.base.interpret())
  }

  @Test def with1Mapping() {
    val v = new IncHashSet[Int]

    val incrementalResult = new IncrementalResult(v.asQueryable.map(_ + 1))
    testFillAndUpdate("IncHashSet & Inc Res(map(_ + 1))", v)
    incrementalResult.interpret() should be (incrementalResult.base.interpret())
  }

  @Test def with2Mapping() {
    val v = new IncHashSet[Int]

    val incrementalResult = new IncrementalResult(v.asQueryable.map(_ + 1).map(_ + 1))
    testFillAndUpdate("IncHashSet & Inc Res(map(_ + 1).map(_ + 1))", v)
    incrementalResult.interpret() should be (incrementalResult.base.interpret())
  }


  @Test def with3Mapping() {
    val v = new IncHashSet[Int]

    val incrementalResult = new IncrementalResult(v.asQueryable.map(_ + 1).map(_ + 1).map(_ + 1))
    testFillAndUpdate("IncHashSet & Inc Res(map(_ + 1).map(_ + 1).map(_ + 1))", v)
    incrementalResult.interpret() should be (incrementalResult.base.interpret())
  }

  @Test def with4Mapping() {
    val v = new IncHashSet[Int]

    val incrementalResult = new IncrementalResult(v.asQueryable.map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1))
    testFillAndUpdate("IncHashSet & Inc Res(map(_ + 1).map(_ + 1).map(_ + 1).map(_ + 1))", v)
    incrementalResult.interpret() should be (incrementalResult.base.interpret())
  }

  @Test def zManyQueries() {
    val v = new IncHashSet[Int]
    val vIncUpd = new IncrementalResult[Int](v)

    val vQueryable: Exp[Traversable[Int]] = v.asQueryable

    val vQueryablePlusOne = vQueryable.map(_ + 1)
    val vQueryablePlusOneIncRes = new IncrementalResult(vQueryablePlusOne)
    val vQueryablePlusOnePlusOne = new IncrementalResult(vQueryablePlusOne.map(_ + 1))
    val vQueryablePOIRPlusOne = new IncrementalResult(vQueryablePlusOneIncRes.asQueryable.map(_ + 1))
    val vIncUpdPlus2 = new IncrementalResult(for (i <- vIncUpd.asQueryable) yield 2 + i)

    benchMark("various queries", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= 1 until 10 * 1000
    }
    vQueryablePlusOneIncRes.interpret() should be (vQueryablePlusOneIncRes.base.interpret())
    benchMark("various queries - upd", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v += 1
    }
  }
}
