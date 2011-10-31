package ivm
package tests

import expressiontree._

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import collections.IncHashSet
import org.junit.{Ignore, Test}
import optimization.Optimization
import ivm.expressiontree.Lifting

/**
 * User: pgiarrusso
 * Date: 31/10/2011
 */


class IVMFlatMapTest1 extends JUnitSuite with ShouldMatchersForJUnit with IVMTestUtil {
  import Lifting._

  def testFlatMap(working: Boolean) {
    //val v = Seq(1, 2, 3)
    val v = IncHashSet[Int]()
    if (!working)
      v ++= Seq(10, 20, 30)
    val v2 = IncHashSet(4, 5, 6)

    val res = new IncrementalResult[Int](for (i <- v.asQueryable; j <- v2.asQueryable) yield i + j)
    show("res", res)
    if (working)
      v ++= Seq(10, 20, 30)
    show("res", res)
    v += 40
    show("res", res)
    v2 += 7
    show("res", res)
  }

  def testFlatMapJoin(working: Boolean) {
    //val v = Seq(1, 2, 3)
    val v = IncHashSet[Int]()
    if (!working)
      v ++= Seq(10, 20, 30)
    val v2 = IncHashSet(20, 40)

    val res = new IncrementalResult[Int](for (i <- v.asQueryable; j <- v2.asQueryable; if 2 * i is j) yield i + j)
    val res2 = new IncrementalResult[Int](Optimization.optimize(for (i <- v.asQueryable; j <- v2.asQueryable; if 2 * i is j) yield i + j))
    def out() {
      show("res", res)
      show("res2", res2)
    }

    out()
    if (working)
      v ++= Seq(10, 20, 30)
    out()
    v += 40
    out()
    v2 += 60
    out()
  }

  def testFlatMap2(working: Boolean) {
    //val v = Seq(1, 2, 3)
    val v = IncHashSet[Int]()
    if (!working)
      v ++= Seq(10, 20, 30)
    val v2 = IncHashSet(4, 5, 6)

    //Illegal term
    //val res = new IncrementalResult[Int](v.asQueryable.flatMap(i => new IncrementalResult(v2.asQueryable.map(j => i + j))))
    val res = new IncrementalResult[Int](v.asQueryable.flatMap(i => v2.asQueryable.map(j => i + j).map(_ + 1)))

    show("res", res)
    if (working)
      v ++= Seq(10, 20, 30)
    show("res", res)
    v += 40
    show("res", res)
    v2 += 7
    show("res", res)
  }

  @Test
  def testFlatMapWorking() {
    testFlatMap(working = true)
  }

  @Test
  def testFlatMapNotWorking() {
    testFlatMap(working = false)
  }

  @Test
  def testFlatMapWorking2() {
    testFlatMap2(working = true)
  }

  @Test
  def testFlatMapNotWorking2() {
    testFlatMap2(working = false)
  }

  @Test def testFlatMapJoinWorking() = testFlatMapJoin(true)
  @Test def testFlatMapJoinNonWorking() = testFlatMapJoin(false)

  @Ignore @Test def flatMap2V1 = testFlatMap2(0)
  @Ignore @Test def flatMap2V2 = testFlatMap2(1)

  def testFlatMap2(version: Int) {
    val v = new IncHashSet[Int]
    v ++= Seq(0, 1, 2)
    val vArr = Array(IncHashSet(40, 50, 60), IncHashSet(40, 50, 60), IncHashSet(40, 50, 60), IncHashSet(40, 50, 60))

    /*
    // This term is exotic because of its use of interpret - which can be avoided, now that map is available on
    // Exp[Traversable[T]], rather than only on QueryReifier[T].
    val resOld = new IncrementalResult[Int](for (i <- v.asQueryable;
                                              j <- liftCall('dontknow,
                                                ((_: Array[IncHashSet[Int]]).apply(_: Int).asQueryable),
                                                vArr, i).interpret()) yield i + j)*/

    //Now that map is available, we can write also this code:
    val resMid = new IncrementalResult[Int](for (i <- v.asQueryable;
                                              j <- liftCall('dontknow,
                                                ((_: Seq[IncHashSet[Int]]).apply(_: Int)),
                                                vArr, i)) yield i + j)
    // But in the last line, we get j <- coll with coll: Exp[IncHashSet[T]], and IncHashSet[T] <: Exp[Traversable[T]].
    // That's bad!

    //This code is yet better.
    val resNew = new IncrementalResult[Int](for (i <- v.asQueryable;
                                              j <- (vArr: Exp[Seq[IncHashSet[Int]]])(i))
                                            yield i + j)

    val res = Seq(resMid, resNew)(version)
    show("res", res)
    v += 3
    show("res", res)
    for (i <- 0 until 3) {
      vArr(i) += 70
      show("res", res)
    }
  }
}