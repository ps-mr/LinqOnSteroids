package ivm
package tests

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.{Ignore, Test}

import expressiontree._

import expressiontree.Lifting._
import optimization.Optimization


/**
* User: pgiarrusso
* Date: 25/8/2011
*/

class SubquerySharingTests extends JUnitSuite with Matchers with AssertionsForJUnit {
  val l: Exp[Seq[(Int, Int)]] = asExp(Vector.range(1, 3).flatMap(i => Vector.range(1, 2).map((i, _))))
  val baseRange = Vector.range(1, 10).asSquopt

  @Ignore //fails because I disabled directsubqueryShare in SubquerySharing
  @Test def testSimpleSharing() {
    val s1 = l.map(p => (p._1 + 1, p._2 + 2))
    val ress1 = s1.eval
    Optimization.addIndex(s1, Some(ress1))

    val q = l.map(p => (p._1 + 1, p._2 + 2)).withFilter(_._1 ==# 5)
    val res = Optimization.shareSubqueries(q)
    res should equal (asExp(ress1).withFilter( _._1 ==# 5))
    res should not equal (q)

    val q2 = l.map(p => (p._1 + 2, p._2 + 1)).withFilter(_._1 ==# 5)
    val res2 = Optimization.shareSubqueries(q2)
    res2 should equal (q2)

    Optimization.removeIndex(s1)
  }

  def shareSubqueriesOpt[T](x: Exp[T]) = Optimization filterToWithFilter (Optimization simplifyFilters (Optimization shareSubqueries x))

  def indexingTest[T: TypeTag, U: TypeTag, TupleT: TypeTag](query: Exp[Seq[T]], idx: Exp[Map[U, TupleT]], fullOptim: Boolean = true)(expectedOptQueryProducer: Exp[Map[U, TupleT]] => Exp[Traversable[T]]) {
    val idxRes = idx.eval
    val expectedOptQuery = expectedOptQueryProducer(idxRes)
    query.eval should be (expectedOptQuery.eval.force) //The call to force makes sure that
    // the expected value in error messages shows the actual collection contents.
    def constantFolding[T](t: Exp[T]) = /*Optimization constantFolding */ t

    Optimization.addIndex(idx, Some(idxRes))
    val optQuery =
      if (fullOptim)
        Optimization.optimize(query)
      else
        constantFolding(shareSubqueriesOpt(query))
    Optimization.removeIndex(idx)

    optQuery should be (Optimization filterToWithFilter (constantFolding(expectedOptQuery)))
    //We assumed that if optQuery == expectedOptQuery, then optQuery.eval == expectedOptQuery.eval == query.eval
    //but manifests could be different!
    optQuery.eval should be (expectedOptQuery.eval)
  }

  object expVector {
    def range(a: Exp[Int], b: Exp[Int]) = globalFmap(a, b)('range, "Vector.range", Vector.range(_, _))
  }

  implicit class ExpRichInt(e: Exp[Int]) {
    def until(end: Exp[Int]): Exp[Range] = fmap(e, end)('until, _ until _)
    def until(end: Int, step: Int): Exp[Range] = fmap(e, end, step)('until, _ until (_, _))
    def to(end: Int): Exp[Range.Inclusive] = fmap(e, end)('to, _ to _)
    def to(end: Int, step: Int): Exp[Range.Inclusive] = fmap(e, end, step)('to, _ to (_, _))
  }

  val l2: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- expVector.range(1, i)
      if (j ==# 5)
    } yield i + j

  @Test def expVectorTest() {
    expVector.range(1, 10).eval should be (Vector.range(1, 10))
    val one = asExp(1)
    (one to 10).eval should be (1 to 10)
    (one until 10).eval should be (1 until 10)
  }

  val l2IdxBase = for {
    i <- baseRange
    j <- expVector.range(1, i)
  } yield (i, j)

  @Test def testComplexIndexing() {
    val l2Idx = l2IdxBase indexBy { _._2 }
    indexingTest(l2, l2Idx){ _(5) map (p => p._1 + p._2) }
  }

  val l3_k: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- expVector.range(1, i)
      k <- expVector.range(1, j)
      if (k ==# 5)
    } yield i + j + k

  //The if will get lifted by optimizations - that's why l3IdxBase_j needs to be used during indexing.
  val l3_j: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- expVector.range(1, i)
      k <- expVector.range(1, j)
      if (j ==# 5)
    } yield i + j + k

  val l3IdxBase_k = for {
    i <- baseRange
    j <- expVector.range(1, i)
    k <- expVector.range(1, j)
  } yield (i, j, k)

  val l3IdxBase_j: Exp[Seq[(Int, Int)]] = for {
    i <- baseRange
    j <- expVector.range(1, i)
  } yield (i, j)

  @Test def testComplexIndexing3Level_j() {
    val l3Idx = l3IdxBase_j indexBy { _._2 }
    //Here p(0) and p(1) have type Exp[Int] but only by chance. If the index were tuple-based, it'd have type Exp[Seq[(Int, Int)]]. Since the tuple members have the same type,
    //the seq-based index has type Exp[Seq[Seq[Int]]].
    //Exp[Seq[(Int, String)]] would become Exp[Seq[Seq[Any]]], and p(0) would then have type Any.
    //indexingTest(l3_j, l3Idx){ _ get 5 flatMap identity flatMap (p => for (k <- expVector.range(1, p(1))) yield p(0) + p(1) + k) }
    indexingTest(l3_j, l3Idx){ _(5) flatMap (p => for (k <- expVector.range(1, p._2)) yield p._1 + p._2 + k) }
    indexingTest(l3_shifted, l3Idx){ _(5) flatMap (p => for (k <- expVector.range(1, p._2)) yield p._1 + p._2 + k) }
  }

  @Test def testComplexIndexing3Level_k() {
    val l3Idx = l3IdxBase_k indexBy { _._3 }
    indexingTest(l3_k, l3Idx){ _(5) map (p => p._1 + p._2 + p._3) }
  }

  val l3_shifted: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- expVector.range(1, i)
      if (j ==# 5)
      k <- expVector.range(1, j)
    } yield i + j + k

  val l3_k1_opt: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- Let(i + 1)
      k <- expVector.range(1, j)
      if (k ==# 5)
    } yield i + j + k

  val l3IdxBase_k1_opt = for {
    i <- baseRange
    j <- Let(i + 1)
    k <- expVector.range(1, j)
  } yield (i, j, k)


  @Test def testComplexIndexing3Level_k1_opt() {
    val l3Idx = l3IdxBase_k1_opt indexBy { _._3 }
    indexingTest(l3_k1_opt, l3Idx){ _(5) map (p => p._1 + p._2 + p._3) }
  }

  val l3_k_opt: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- Let(i + 1) if (j ==# 2)
      k <- Let(i + j + 2)
      if (k ==# 5)
    } yield i + j + k


  @Test def testComplexIndexing3Level_k_opt() {
    val l3IdxBase_k_opt = for {
      i <- baseRange
      j <- Let(i + 1)
      k <- Let(i + j + 2)
    } yield (i, j, k)

    val l3Idx = l3IdxBase_k_opt indexBy { _._3 }
    indexingTest(l3_k_opt, l3Idx){ idx => (idx(5) filter (_._2 ==# 2) map (p => p._1 + p._2 + p._3)) }
  }

  @Test def testComplexIndexing3Level_k_opt_workaround() {
    val l3IdxBase_k_opt = for {
      i <- baseRange
      j <- asExp(Seq(i + 1))
      k <- Seq(i + j + 2): Exp[Iterable[Int]]
    } yield (i, j, k)

    val l3Idx = l3IdxBase_k_opt indexBy { _._3 }
    indexingTest(l3_k_opt, l3Idx){ idx => (idx(5) filter (_._2 ==# 2) map (p => p._1 + p._2 + p._3)) }
  }

  val l3_k_seqlet: Exp[Seq[Int]] =
    for {
      i <- baseRange
      j <- asExp(Seq(i + 1))
      k <- asExp(Seq(i + j + 2))
      if (k ==# 5)
    } yield i + j + k

  @Test def testComplexIndexing3Level_k_seqlet() {
    val l3IdxBase_k_seqlet = for {
      i <- baseRange
      j <- asExp(Seq(i + 1))
      k <- asExp(Seq(i + j + 2))
    } yield (i, j, k)

    val l3Idx = l3IdxBase_k_seqlet indexBy { _._3 }
    indexingTest(l3_k_seqlet, l3Idx){ _(5) map (p => p._1 + p._2 + p._3) }
  }

  @Test def testComplexIndexing3Level_k_seqlet_workaround() {
    val l3IdxBase_k_seqlet = for {
      i <- baseRange
      j <- asExp(Seq(i + 1))
      k <- Seq(i + j + 2): Exp[Iterable[Int]]
    } yield (i, j, k)

    val l3Idx = l3IdxBase_k_seqlet indexBy { _._3 }
    indexingTest(l3_k_seqlet, l3Idx){ _(5) map (p => p._1 + p._2 + p._3) }
  }

  @Test def testIndexing() {
    val index = l.indexBy(p => p._1 + p._2)
    val testquery = l.withFilter(p => p._1 + p._2 ==# 5)
    indexingTest(testquery, index, false) {_(5)}
  }

  @Test def testIndexingQueryNorm() {
    val index = l.indexBy(p => p._1 + p._2)
    val testquery = l.withFilter(p => p._2 + p._1 ==# 5)
    indexingTest(testquery, index, false) {_(5)}
  }

  @Test def testIndexingIndexNorm() {
    val index = l.indexBy(p => p._2 + p._1) //The index should be normalized, test this
    val testquery = l.withFilter(p => p._1 + p._2 ==# 5)
    indexingTest(testquery, index, false) {_(5)}
  }

  @Test def testCNFconversion() {
    val index = l.indexBy(p => p._1 + p._2)
    val testquery = l.withFilter(p => p._1 <= 7 && p._1 + p._2 ==# 5)
    indexingTest(testquery, index, false) {_(5)  withFilter (p => p._1 <= 7)}
  }


  @Test def testRemoveSubquery() {
    val index = l.indexBy(p => p._1 + p._2)
    Optimization.resetSubqueries()
    Optimization.addIndex(index)
    Optimization.removeIndex(index)
    Optimization.subqueries should be (Map.empty)
  }
}
