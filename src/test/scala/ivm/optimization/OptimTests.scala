package ivm
package optimization

import expressiontree.Lifting._
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree.{Const, Plus, Fun, Exp}

/**
 * User: pgiarrusso
 * Date: 2/1/2012
 */

class OptimTests extends JUnitSuite with ShouldMatchersForJUnit {
  val x = FuncExp.gensym[Int]()

  def testIdempotence[T](e: Exp[T]) = {
    val opt = Optimization.reassociateOps(e)
    opt should be (Optimization.reassociateOps(opt))
    opt
  }

  @Test
  def reassociateOpsF() {
    def f(e: Exp[Int]) = e + 1
    val composedF = f(f(f(x)))

    val optF = testIdempotence(composedF)
    optF should be (Plus(Const(3), x))
  }

  @Test
  def reassociateOpsG() {
    def g(e: Exp[Int]) = 1 + e
    val composedG = g(g(g(x)))
    val optG = testIdempotence(composedG)
    optG should be (Plus(Const(3), x))
  }

  @Test
  def reassociateOpsH() {
    val h = 1 + (1 + x) + 1
    val optH = testIdempotence(h)
    optH should be (Plus(Const(3), x))
  }

  //Optimization results below are not the best, but it's hard to get this kind of patterns right in general
  //(consider increasing distance between constants)
  @Test
  def reassociateOpsI() {
    val i = x + x + 1 + x + 1
    val optI = testIdempotence(i)
    optI should be (Plus(Plus(Plus(Const(2), x), x), x))
  }

  @Test
  def reassociateOpsJ() {
    val j = x + 1 + x + 1
    val optJ = testIdempotence(j)
    optJ should be (Plus(Plus(Const(2), x), x))
  }

  @Test
  def testHoistFilter() {
    val base = Vector.range(1, 6).asSmart
    val query =
      for {
        i <- base
        j <- base
        if i < 3
      } yield (i, j)
    val opt = Optimization.handleFilters(query)
    query.interpret() should be (for (i <- 1 to 2; j <- 1 to 5) yield (i, j))
    opt.interpret() should be (query.interpret())
    opt should be (
      for {
        i <- base
        if i < 3
        j <- base
      } yield (i, j)
    )

    val query2 =
      for {
        i <- base
        j <- base
        k <- base
        l <- base
        if i < 3
      } yield (i, j, l)
    val opt2 = Optimization.handleFilters(query2)
    query2.interpret() should be (for (i <- 1 to 2; j <- 1 to 5; k <- 1 to 5; l <- 1 to 5) yield (i, j, l))
    opt2 should be (
      for {
        i <- base
        if i < 3
        j <- base
        k <- base
        l <- base
      } yield (i, j, l)
    )
    opt2.interpret() should be (query2.interpret())

    val opt21 = Optimization.handleFilters(opt2)
    opt21 should be (
      for {
        i <- base
        if i < 3
        j <- base
        k <- base
        l <- base
      } yield (i, j, l)
    )
    opt21.interpret() should be (query2.interpret())
  }

  val baseCol = Seq(1) asSmart

  @Test
  def testRemoveRedundantOption() {
    val query0 = for (i <- baseCol; j <- i.ifInstanceOf[Int] if j % 2 ==# 1) yield j
    val query = for (i <- baseCol.typeFilter[Int]; j <- Let(i) if j % 2 ==# 1) yield j
    Optimization.toTypeFilter(query0) should be (query)
    val opt = Optimization.removeRedundantOption(query)
    opt.interpret() should be (query.interpret())
    opt should be (for (i <- baseCol.typeFilter[Int]; if i % 2 ==# 1) yield i)
  }

  @Test
  def testIfInstanceOfAndTypeFilterEq() {
    val v = FuncExp.gensym[Int]()
    v.ifInstanceOf[Int] should not be v.ifInstanceOf[Long]
    baseCol.typeFilter[Int] should not be (baseCol.typeFilter[Long])
  }

  @Test
  def testMapToFlatMapAndBack() {
    val baseRange = (1 to 10) asSmart
    val query = for (i <- baseRange) yield i
    import Optimization._
    val transf = mapToFlatMap(query)
    transf should be ((baseRange) flatMap (Seq(_)))
    flatMapToMap(transf) should be (query)
  }

  @Test
  def testMapToFlatMapAndBackOpt() {
    val query = for (i <- Let(1)) yield i
    import Optimization._
    val transf = mapToFlatMap(query)
    transf should be ((Let(1)) flatMap (Let(_)))
    flatMapToMap(transf) should be (query)
  }

  @Test
  def unnestingFromPaper() {
    val Xquery = for {
      x <- (1 to 10).asSmart
      y <- (1 to 10).asSmart
      if y % 2 ==# 0
    } yield (x, y)
    //This is an example where map fusion is applicable, but is a rather stupid case.
    val Yquery = for {
      pair <- Xquery
    } yield (pair._1, pair._2 + 1)

    Yquery.optimize should be (for {
      x <- (1 to 10).asSmart
      y <- (1 to 10).asSmart
      if y % 2 ==# 0
    } yield (x, 1 + y))

    //this example works now even in practice! Making it work requires various forms of fusion and inlining, together with filter hoisting.
    val Zquery = for {
      pair <- Xquery
      if (pair._1 % 2 ==# 0)
    } yield pair
    Zquery.optimize should be (for {
      x <- (1 to 10).asSmart
      if x % 2 ==# 0
      y <- (1 to 10).asSmart
      if y % 2 ==# 0
    } yield (x, y))
  }
}
