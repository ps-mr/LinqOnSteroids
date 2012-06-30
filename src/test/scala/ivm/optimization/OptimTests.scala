package ivm
package optimization

import squopt._
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree.{Const, Plus, Fun, Exp}
import tests.TestUtil
import collection.generic.CanBuildFrom

/**
 * User: pgiarrusso
 * Date: 2/1/2012
 */

class OptimTests extends JUnitSuite with ShouldMatchersForJUnit with TestUtil {
  val x = Fun.gensym[Int]()

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
    val v = Fun.gensym[Int]()
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

    Yquery.optimize should be ((for {
      x <- (1 to 10).asSmart
      y <- (1 to 10).asSmart
      if y % 2 ==# 0
    } yield (x, 1 + y)).optimize)

    //this example works now even in practice! Making it work requires various forms of fusion and inlining, together with filter hoisting.
    val Zquery = for {
      pair <- Xquery
      if (pair._1 % 2 ==# 0)
    } yield pair
    val optZ = Zquery.optimize
    showExp(Zquery)
    showExp(optZ)
    optZ should be (for {
      x <- (1 to 10).asSmart
      if x % 2 ==# 0
      y <- (1 to 10).asSmart
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
  import OptimizationUtil._

  def isCbfCommutative[From, Elem, To](cbf: CanBuildFrom[From, Elem, To]) = {
    val cbfStaticResult = cbf.apply()
    cbfStaticResult.isInstanceOf[collection.Set[_]]
  }

  def isCbfIdempotent[From, Elem, To](cbf: CanBuildFrom[From, Elem, To]) = {
    val cbfStaticResult = cbf.apply()
    cbfStaticResult.isInstanceOf[collection.Set[_]] || cbfStaticResult.isInstanceOf[collection.Map[_, _]]
  }

  //Finally, correct unnesting.
  val existsUnnester3Val: Exp[_] => Exp[_] = {
    case fm @ FlatMap(Filter(c0, f@FuncExpBody(Not(IsEmpty(Filter(c, p))))), fmFun) =>
      //val restQuery = Fun.makefun(fmFun(f.x), p.x)
      val restQuery = Fun[Any, Traversable[Any]](x => fmFun(f.x))
      if (isCbfIdempotent(fm.c))
        //Use the standard rule to unnest exists into a set comprehension
      //stripView(c0) flatMap Fun.makefun((stripView(c) filter p flatMap restQuery)(collection.breakOut): Exp[Traversable[Any]], f.x)
        stripView(c0) flatMap Fun.makefun(stripView(c) filter p flatMap restQuery, f.x)
      else
      //Use an original rule to unnest exists into a non-idempotent comprehension while still doing the needed duplicate
      //elimination
      //breakOut is used to fuse manually the mapping step with toSet. This fusion should be automated!!!
      //Also, we should make somehow sure that breakOut steps are preserved - they currently aren't!
        stripView(c0) flatMap Fun.makefun((((stripView(c) map p)(collection.breakOut): Exp[Set[Boolean]]) filter identity flatMap restQuery)(collection.breakOut): Exp[Traversable[Any]], f.x)
    case e => e
  }

  def existsUnnester2[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.existsUnnester2)
  def existsUnnester3[T](exp: Exp[T]): Exp[T] = exp.transform(existsUnnester3Val)
  def resimpl[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.resimpl)

  def testRenestingExists[T](e: Exp[T]) {
    import Optimization._
    val e1 = mapToFlatMap(e)
    existsRenester(existsUnnester2(e1)) should be (e1)
  }

  def testRenestingExists2[T](e: Exp[T]) {
    import Optimization._
    val e1 = mapToFlatMap(removeIdentityMaps(e))
    println(e1)
    val unnested = existsUnnester(e1)
    println(unnested)
    unnested.interpret() should be (e1.interpret())
    val e2 = existsRenester(mapToFlatMap(unnested))
    println(e2)
    val e3 = generalUnnesting(e2)
    val e4 = resimpl(e3)
    e4 should be (e1)
  }

  def testRenestingExists3[T](e: Exp[T]) {
    import Optimization._
    val e1 = mapToFlatMap(removeIdentityMaps(e))
    println(e1)
    val unnested = existsUnnester3(e1)
    println(unnested)
    unnested.interpret() should be (e1.interpret())
    val e2 = existsRenester(mapToFlatMap(unnested))
    println(e2)
    val e3 = generalUnnesting(e2)
    val e4 = resimpl(e3)
    e4 should be (e1)
  }

  @Test def renestExists1() {
    val compr = for { x <- Set(1, 2, 3, 4) asSmart; if ((1 to 10) ++ (1 to 10)).asSmart exists (y => y * 2 ==# x) } yield x
    testRenestingExists(compr)
    testRenestingExists3(compr)
  }
  @Test def renestExists2() {
    val compr = for { x <- Seq(1, 2, 3, 4) asSmart; if ((1 to 10) ++ (1 to 10)).asSmart exists (y => y * 2 ==# x) } yield x
    testRenestingExists2(compr)
    testRenestingExists3(compr)
  }
}
