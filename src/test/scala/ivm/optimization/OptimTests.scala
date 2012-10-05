package ivm
package optimization

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.{Ignore, Test}
import expressiontree._
import Lifting._
import tests.TestUtil
import collection.generic.CanBuildFrom

/**
 * User: pgiarrusso
 * Date: 2/1/2012
 */

class OptimTests extends JUnitSuite with ShouldMatchersForJUnit with TestUtil {
  val x: Exp[Int] = Fun.gensym[Int]()

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
    optF should be (Sym(Plus(Const(3), x)))
  }

  @Test
  def reassociateOpsG() {
    def g(e: Exp[Int]) = 1 + e
    val composedG = g(g(g(x)))
    val optG = testIdempotence(composedG)
    optG should be (Sym(Plus(Const(3), x)))
  }

  @Test
  def reassociateOpsH() {
    val h = 1 + (1 + x) + 1
    val optH = testIdempotence(h)
    optH should be (Sym(Plus(Const(3), x)))
  }

  //Optimization results below are not the best, but it's hard to get this kind of patterns right in general
  //(consider increasing distance between constants)
  @Test
  def reassociateOpsI() {
    val i = x + x + 1 + x + 1
    val optI = testIdempotence(i)
    optI should be (Sym(Plus(Plus(Plus(Const(2), x), x), x)))
  }

  @Test
  def reassociateOpsJ() {
    val j = x + 1 + x + 1
    val optJ = testIdempotence(j)
    optJ should be (Sym(Plus(Plus(Const(2), x), x)))
  }

  @Test
  def testHoistFilter() {
    def optim[T](exp: Exp[T]) = Optimization flatMapToMap (Optimization handleFilters (Optimization mapToFlatMap exp))
    val base = Vector.range(1, 6).asSquopt
    val query =
      for {
        i <- base
        j <- base
        if i < 3
      } yield (i, j)
    val opt = optim(query)
    query.eval should be (for (i <- 1 to 2; j <- 1 to 5) yield (i, j))
    opt.eval should be (query.eval)
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
    val opt2 = optim(query2)
    query2.eval should be (for (i <- 1 to 2; j <- 1 to 5; k <- 1 to 5; l <- 1 to 5) yield (i, j, l))
    opt2 should be (
      for {
        i <- base
        if i < 3
        j <- base
        k <- base
        l <- base
      } yield (i, j, l)
    )
    opt2.eval should be (query2.eval)

    val opt21 = optim(opt2)
    opt21 should be (
      for {
        i <- base
        if i < 3
        j <- base
        k <- base
        l <- base
      } yield (i, j, l)
    )
    opt21.eval should be (query2.eval)
  }

  val baseCol = Seq(1).asSquopt

  @Test
  def testRemoveRedundantOption() {
    val query0 = for (i <- baseCol; j <- i.ifInstanceOf[Int] if j % 2 ==# 1) yield j
    val query = for (i <- baseCol.typeFilter[Int]; j <- Let(i) if j % 2 ==# 1) yield j
    Optimization.toTypeFilter(query0) should be (query)
    val opt = Optimization.removeRedundantOption(query)
    opt.eval should be (query.eval)
    opt should be (for (i <- baseCol.typeFilter[Int]; if i % 2 ==# 1) yield i)
  }

  @Test
  def testIfInstanceOfAndTypeFilterEq() {
    val v: Exp[Int] = Fun.gensym[Int]()
    v.ifInstanceOf[Int] should not be v.ifInstanceOf[Long]
    baseCol.typeFilter[Int] should not be (baseCol.typeFilter[Long])
  }

  @Test
  def testMapToFlatMapAndBack() {
    val baseRange = (1 to 10).asSquopt
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

  val Xquery = for {
    x <- (1 to 10).asSquopt
    y <- (1 to 10).asSquopt
    if y % 2 ==# 0
  } yield (x, y)

  @Test
  def unnestingFromPaperPart1() {
    //This is an example where map fusion is applicable, but is a rather stupid case.
    val Yquery = for {
      pair <- Xquery
    } yield (pair._1, pair._2 + 1)

    val expected = (for {
      x <- (1 to 10).asSquopt
      y <- (1 to 10).asSquopt
      if y % 2 ==# 0
    } yield (x, 1 + y)).optimize
    Optimization filterToWithFilter (Optimization preIndexing Yquery) should be (expected)
    Yquery.optimize should be (expected)
  }

  @Test
  def unnestingFromPaperPart2() {
    //this example works now even in practice! Making it work requires various forms of fusion and inlining, together with filter hoisting.
    val Zquery = for {
      pair <- Xquery
      if (pair._1 % 2 ==# 0)
    } yield pair
    //val optZ = Zquery.optimize
    val optZ = Optimization.preIndexing(Zquery)
    showExp(Zquery, "Zquery")
    showExp(optZ, "optZ")
    optZ should be (for {
      x <- (1 to 10).asSquopt
      if x % 2 ==# 0
      y <- (1 to 10).asSquopt
      if y % 2 ==# 0
    } yield (x, y))
  }
  /*{
    Filter(
      ConstByIdentity(List(1, 2, 3)...),
      v112 => Not(IsEmpty(
        FlatMap(
          ConstByIdentity(Vector(1, 2, 3)...),
          v116 =>
            Filter(ExpSeq(List(Eq(Times(Const(2),v116),v112))),v115 => v115)
      ))))
  }*/

  import expressiontree._

  def testRenestingExistsForSets[T](e: Exp[T]) {
    import Optimization._
    val e1 = mapToFlatMap(e) //XXX: mapToFlatMap preserves the Set-ness of the expression, by preserving the
    // CanBuildFrom, but that's an exception.
    existsRenester(existsUnnester(e1)) should be (e1)
  }

  def testRenestingExistsGeneric[T: TypeTag](e: Exp[T]) {
    import Optimization._
    val e1 = mapToFlatMap(removeIdentityMaps(e))
    println(e1)
    val unnested = existsUnnester(e1)
    println(unnested)
    unnested.eval should be (e1.eval)
    val e2 = existsRenester(mapToFlatMap(unnested))
    println(e2)
    val e3 = generalUnnesting(e2)
    val e4 = resimplFilterIdentity(e3)
    e4 should be (e1)
    preIndexing(e4) should be (reassociateOps(e1))
  }

  @Test def renestExists1() {
    val compr = for { x <- Set(1, 2, 3, 4).asSquopt; if ((1 to 10) ++ (1 to 10)).asSquopt exists (y => y * 2 ==# x) } yield x
    testRenestingExistsForSets(compr)
    testRenestingExistsGeneric(compr)
  }
  @Test def renestExists2() {
    val compr = for { x <- Seq(1, 2, 3, 4).asSquopt; if ((1 to 10) ++ (1 to 10)).asSquopt exists (y => y * 2 ==# x) } yield x
    testRenestingExistsGeneric(compr)
  }
  @Test def testCompilationTypes() {
    val compr = for { x <- Seq[Long](1, 2, 3, 4).asSquopt; if ((1 to 10) ++ (1 to 10)).asSquopt exists (y => y * 2 ==# x) } yield x
    testRenestingExistsGeneric(compr)
  }
}
