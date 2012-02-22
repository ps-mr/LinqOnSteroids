package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test

import expressiontree._

import expressiontree.Lifting._
import optimization.Optimization


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class SubquerySharingTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: Exp[Seq[(Int, Int)]] = asExp(Vector.range(1, 3).flatMap(i => Vector.range(1, 2).map((i, _))))

  @Test def testSimpleSharing() {
    val s1 = l.map(p => (p._1 + 1, p._2 + 2))
    val ress1 = s1.interpret()
    Optimization.addSubQuery(s1)

    val q = l.map(p => (p._1 + 1, p._2 + 2)).withFilter(_._1 === 5)
    val res = Optimization.shareSubqueries(q)
    res should not equal (q)
    res should equal (Const(ress1).withFilter( _._1 is 5))

    val q2 = l.map(p => (p._1 + 2, p._2 + 1)).withFilter(_._1 === 5)
    val res2 = Optimization.shareSubqueries(q2)
    res2 should equal (q2)

    Optimization.removeSubQuery(s1)
  }

  @Test def testComplexIndexing() {
    val l2: Exp[Seq[Int]] =
      for {
        i <- Vector.range(1, 10).asSmartCollection
        j <- onExp(i)('Vector$range_1, Vector.range(1, _))
        if (j === 5)
      } yield i + j

    val l2IdxBase = for {
      i <- Vector.range(1, 10).asSmartCollection
      j <- onExp(i)('Vector$range_1, Vector.range(1, _))
    } yield (i, j)

    val l2Idx = l2IdxBase groupBy { _._2 }
    val optQueryExpected = l2Idx(5) map (p => p._1 + p._2)
    l2.interpret() should be (optQueryExpected.interpret())
    Optimization.addSubQuery(l2Idx)
    val optQuery = Optimization.shareSubqueries(l2)
    Optimization.removeSubQuery(l2Idx)

    optQuery should be (optQueryExpected)
  }

  @Test def testIndexing {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubQuery(index)

    val testquery = l.withFilter(p => p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubQuery(index)
  }

  @Test def testIndexingQueryNorm() {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubQuery(index)

    val testquery = l.withFilter(p => p._2 + p._1 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubQuery(index)
  }

  @Test def testIndexingIndexNorm() {
    val index = l.groupBy(p => p._2 + p._1) //The index should be normalized, test this
    val indexres = index.interpret()
    Optimization.addSubQuery(index)

    val testquery = l.withFilter(p => p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubQuery(index)
  }

  @Test def testCNFconversion {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubQuery(index)

    val testquery = l.withFilter(p => p._1 <= 7 && p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)).withFilter(p => p._1 <= 7))

    Optimization.removeSubQuery(index)
  }


  @Test def testRemoveSubquery() {
    val index = l.groupBy(p => p._1 + p._2)
    Optimization.addSubQuery(index)
    Optimization.removeSubQuery(index)
    Optimization.subqueries should be (Map.empty)
  }
}
