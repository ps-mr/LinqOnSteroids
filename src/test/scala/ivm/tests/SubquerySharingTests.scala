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
    Optimization.addSubquery(s1)

    val q = l.map(p => (p._1 + 1, p._2 + 2)).withFilter(_._1 === 5)
    val res = Optimization.shareSubqueries(q)
    res should not equal (q)
    res should equal (Const(ress1).withFilter( _._1 is 5))

    val q2 = l.map(p => (p._1 + 2, p._2 + 1)).withFilter(_._1 === 5)
    val res2 = Optimization.shareSubqueries(q2)
    res2 should equal (q2)

    Optimization.removeSubquery(s1)
  }

  def indexingTest[T, U, V](query: Exp[Seq[T]], idx: Exp[Map[U, Seq[V]]])(expectedOptQueryProducer: Map[U, Seq[V]] => Exp[Seq[T]]) {
    val idxRes = idx.interpret()
    val expectedOptQuery = expectedOptQueryProducer(idxRes)
    query.interpret() should be (expectedOptQuery.interpret())

    Optimization.addSubquery(idx)
    val optQuery = Optimization.shareSubqueries(query)
    Optimization.removeSubquery(idx)

    optQuery should be (expectedOptQuery)
  }

  val l2: Exp[Seq[Int]] =
    for {
      i <- Vector.range(1, 10).asSmartCollection
      j <- onExp(i)('Vector$range_1, Vector.range(1, _))
      if (j === 5)
    } yield i + j

  val l2IdxBase = for {
    i <- Vector.range(1, 10).asSmartCollection
    j <- onExp(i)('Vector$range_1, Vector.range(1, _))
  } yield Seq(i, j)

  @Test def testComplexIndexing() {
    val l2Idx = l2IdxBase groupBy { _(1) }
    indexingTest(l2, l2Idx){ asExp(_)(5) map (p => p(0) + p(1)) }
  }

  val l3_k: Exp[Seq[Int]] =
    for {
      i <- Vector.range(1, 10).asSmartCollection
      j <- onExp(i)('Vector$range_1, Vector.range(1, _))
      k <- onExp(j)('Vector$range_1, Vector.range(1, _))
      if (k === 5)
    } yield i + j + k

  val l3_j: Exp[Seq[Int]] =
    for {
      i <- Vector.range(1, 10).asSmartCollection
      j <- onExp(i)('Vector$range_1, Vector.range(1, _))
      k <- onExp(j)('Vector$range_1, Vector.range(1, _))
      if (j === 5)
    } yield i + j + k

  val l3IdxBase = for {
    i <- Vector.range(1, 10).asSmartCollection
    j <- onExp(i)('Vector$range_1, Vector.range(1, _))
    k <- onExp(j)('Vector$range_1, Vector.range(1, _))
  } yield Seq(i, j, k)

  @Test def testComplexIndexing3Level_j() {
    val l3Idx = l3IdxBase groupBy { _(1) }
    indexingTest(l3_j, l3Idx){ asExp(_)(5) map (p => p(0) + p(1) + p(2)) }
  }

  @Test def testComplexIndexing3Level_k() {
    val l3Idx = l3IdxBase groupBy { _(2) }
    indexingTest(l3_k, l3Idx){ asExp(_)(5) map (p => p(0) + p(1) + p(2)) }
  }

  val l3_shifted: Exp[Seq[Int]] =
    for {
      i <- Vector.range(1, 10).asSmartCollection
      j <- onExp(i)('Vector$range_1, Vector.range(1, _))
      if (j === 5)
      k <- onExp(j)('Vector$range_1, Vector.range(1, _))
    } yield i + j + k

  @Test def testComplexIndexing3Level_shifted() {
    val l3Idx = l3IdxBase groupBy { _(1) }
    indexingTest(l3_shifted, l3Idx){ asExp(_)(5) map (p => p(0) + p(1) + p(2)) }
  }

  @Test def testIndexing {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubquery(index)

    val testquery = l.withFilter(p => p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubquery(index)
  }

  @Test def testIndexingQueryNorm() {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubquery(index)

    val testquery = l.withFilter(p => p._2 + p._1 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubquery(index)
  }

  @Test def testIndexingIndexNorm() {
    val index = l.groupBy(p => p._2 + p._1) //The index should be normalized, test this
    val indexres = index.interpret()
    Optimization.addSubquery(index)

    val testquery = l.withFilter(p => p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)))

    Optimization.removeSubquery(index)
  }

  @Test def testCNFconversion {
    val index = l.groupBy(p => p._1 + p._2)
    val indexres = index.interpret()
    Optimization.addSubquery(index)

    val testquery = l.withFilter(p => p._1 <= 7 && p._1 + p._2 === 5)
    val optimized = Optimization.shareSubqueries(testquery)
    optimized should equal (App(Const(indexres), Const(5)).withFilter(p => p._1 <= 7))

    Optimization.removeSubquery(index)
  }


  @Test def testRemoveSubquery() {
    val index = l.groupBy(p => p._1 + p._2)
    Optimization.addSubquery(index)
    Optimization.removeSubquery(index)
    Optimization.subqueries should be (Map.empty)
  }
}
