package ivm
package performancetests

import org.scalatest.junit.{JUnitSuite, ShouldMatchersForJUnit}
import org.junit.{Ignore, Test}
import collection.mutable

import mutable.Buffer
import tests.IVMTestUtil

import expressiontree._
import Lifting._
import collections.{IncArrayBuffer, IncHashSet}
import collection.generic.{Growable, Shrinkable}
import optimization.Optimization

/**
 * User: pgiarrusso
 * Date: 2/11/2011
 */

/*
class IVMPerformanceTests extends JUnitSuite with ShouldMatchersForJUnit with IVMTestUtil with Benchmarking {
  override val warmUpLoops = 100
  override val sampleLoops = 50
  val maxN = if (debugBench) 9 else 17
  val mapCounts = (1 until (maxN, 4)) :+ maxN

  val toAdd = Array[Int]((1 to 10 * 1000): _*)
  val updSize = 1000
  val toAddDel = if (debugBench) Array(1) else Array[Int]((100 * 1000 to 100 * 1000 + updSize): _*)

  @Test
  def nativeMappingHashSet() {
    val v = new mutable.HashSet[Int]
    var res: mutable.HashSet[Int] = null
    benchMark("nativeMap on HashSet", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= toAdd
      res = v map (_ + 1)
    }
    benchMark("nativeMap on HashSet - update and recompute", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= toAdd
      v ++= toAddDel
      res = v map (_ + 1)
    }
  }

  @Test
  def nativeMappingArrayBuffer() {
    val v = new mutable.ArrayBuffer[Int]
    var res: mutable.ArrayBuffer[Int] = null
    benchMark("nativeMap on ArrayBuffer", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= toAdd
      res = v map (_ + 1)
    }
    benchMark("nativeMap on ArrayBuffer - update and recompute", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v ++= toAddDel
      res = v map (_ + 1)
      //XXX: use code comparable to testFillAndUpdateArrayBuffer - or actually do the proper fix.
      //v trimEnd toAddDel.size
      val lastPos = v.size - toAddDel.size
      for (i <- 0 until toAddDel.size) v remove lastPos
    }
  }

  def testFillAndUpdateHashSet(title: String, v: Growable[Int] with Shrinkable[Int]) {
    benchMark(title, warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= toAdd
    }
    benchMark(title + " - upd", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v ++= toAddDel
      v --= toAddDel
    }
    println()
  }

  def testFillAndUpdateArrayBuffer(title: String, v: Buffer[Int]) {
    benchMark(title, warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v.clear()
      v ++= toAdd
    }
    benchMark(title + " - upd", warmUpLoops = warmUpLoops, sampleLoops = sampleLoops) {
      v ++= toAddDel
      //v trimEnd toAddDel.size
      //XXX: again, ArrayBuffer is too "optimized" and does not propagate change - for now inline the standard defs
      //of trimEnd and remove(Int, Int).
      val lastPos = v.size - toAddDel.size
      for (i <- 0 until toAddDel.size) v remove lastPos
    }
    println()
  }

  @Test def incHashSet() {
    val v = new IncHashSet[Int]
    testFillAndUpdateHashSet("IncHashSet", v)
  }

  @Test def incHashSetAndIncRes() {
    val v = new IncHashSet[Int]
    val incrementalResult = new IncrementalResult[Int](v)
    testFillAndUpdateHashSet("IncHashSet & IncRes", v)
    checkIncRes(incrementalResult)
  }

  //First results show that map fusion reduces performance. After adding a specialized version of constant-folding (an hack)
  @Test def withNMappingsArrOptimize = withNMappingsArr(true)
  @Test def withNMappingsArrNoOptimize = withNMappingsArr(false)
  @Test def withNMappingsArrNoOptimizeMap2 = withNMappingsArr(false, true)
  @Test def withNMappingsArrOptimizeMap2 = withNMappingsArr(true, true)

  def withNMappingsArr(optimize: Boolean, useMap2: Boolean = false) {
    println(); println()
    for (n <- mapCounts) {
      val v = new IncArrayBuffer[Int]

      var query = v.asQueryable
      for (i <- 1 to n) {
        if (useMap2)
          query = query.map2(_ + 1)
        else
          query = query.map(_ + 1)
      }
      if (optimize)
        query = Optimization.optimize(query)
      val incrementalResult = new IncrementalResult(query)
      println(query)
      testFillAndUpdateArrayBuffer("IncArrayBuffer & Inc Res(%d times map(_ + 1)) - opt %b, useMap2 %b" format
        (n, optimize, useMap2), v)
      incrementalResult.interpret() should be (incrementalResult.base.interpret().toSet)
    }
  }

  @Test def withNMappings() {
    println(); println()
    for (n <- mapCounts) {
      val v = new IncHashSet[Int]

      var query = v.asQueryable
      for (i <- 1 to n) {
        query = query.map(_ + 1)
      }
      val incrementalResult = new IncrementalResult(query)
      testFillAndUpdateHashSet("IncHashSet & Inc Res(%d times map(_ + 1))" format n, v)
      checkIncRes(incrementalResult)
    }
  }

  @Test def zManyQueries() {
    println(); println()
    val v = new IncHashSet[Int]
    val vIncUpd = new IncrementalResult[Int](v)

    val vQueryable: Exp[Traversable[Int]] = v.asQueryable

    val vQueryablePlusOne = vQueryable.map(_ + 1)
    val vQueryablePlusOneIncRes = new IncrementalResult(vQueryablePlusOne)
    val vQueryablePlusOnePlusOne = new IncrementalResult(vQueryablePlusOne.map(_ + 1))
    val vQueryablePOIRPlusOne = new IncrementalResult(vQueryablePlusOneIncRes.asQueryable.map(_ + 1))
    val vIncUpdPlus2 = new IncrementalResult(for (i <- vIncUpd.asQueryable) yield 2 + i)

    testFillAndUpdateHashSet("various queries", v)
    checkIncRes(vQueryablePlusOneIncRes)
    checkIncRes(vQueryablePlusOnePlusOne)
    checkIncRes(vQueryablePOIRPlusOne)
    checkIncRes(vIncUpdPlus2)
  }
}
*/
