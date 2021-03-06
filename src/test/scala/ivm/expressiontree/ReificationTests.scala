package ivm
package expressiontree

import scala.collection.immutable.Vector
import Lifting._
import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.{Ignore, Test}
import optimization.Optimization
import Optimization.optimize

class ReificationTests extends JUnitSuite with AssertionsForJUnit with Matchers {
  def test(x: Int, y: Int): Boolean = x+y == 12
  def foo(x: Int): Int = x
  def bar(x: Int): Int = x
  val l: Exp[Traversable[Int]] = pure(Vector.range(1,10))
  val j: Exp[Traversable[Int]] = pure(Vector.range(1,10))
  val q = for (k  <- l if k <= 5) yield 3 + k
  val r = for (k  <- l; k2  <- j if k ==# k2) yield k + k2
  val r1 = for (k <- l; k2 <- j if k + k2 ==# k2 + k) yield k + k2
  val r2 = for (k <- l; k2 <- j if fmap(k, k2)('test$Int$Int, test)) yield k + k2
  val r3 = for (k <- l; k2 <- j if fmap(k)('foo$Int, foo) ==# fmap(k2)('bar$Int, bar)) yield (k, k2)

  @Ignore @Test
  def testq() {
    optimize(q) should equal (q)
  }
  @Test
  def testr() {
    optimize(r) should equal (l.join(j)(x => x, y => y, p => (p._1 + p._2)))
  }

  @Ignore @Test
  def testr1() {
    optimize(r1) should equal (r1)
  }

  @Ignore @Test
  def testr2() {
    optimize(r2) should equal (r2)
  }

  @Test
  def testr3() {
    optimize(r3) should equal (l.join(j)(x => fmap(x)('foo$Int,foo), y => fmap(y)('bar$Int, bar), p => (p._1, p._2)))
  }

  @Test
  def testfreeVars() {
    q.freeVars should be (Set.empty)
    newMapOp(l,Fun( (x: Exp[Int]) => x + Var(58))).freeVars should be (Set(Var(58)))
  }

  @Test
  def testReification() {
    q should equal (newMapOp[Int, Traversable[Int], Int, Traversable[Int]](newWithFilter(l, Fun((v2: Exp[Int]) => LEq(v2, 5))),Fun((v3: Exp[Int]) => Plus(3, v3))))
    /*println(q.exec())
    println(optimize(q).eval.exec())
    //val r = l.flatMap( (k) => j.withFilter( (k2 ) => k ==# k2).map( (k2:Exp[Int]) => k+k2))
    println(r);
    println(optimize(r))
    println(r.exec())
    println(optimize(r).eval.exec())
    println(r1);
    println(optimize(r1))
    println(r.exec())
    println(r2);
    println(optimize(r2))
    println(r.exec())
    println(r3);
    println(optimize(r3))
    println(r3.exec())
    println(optimize(r3).eval.exec())*/
  }

  @Test
  def testAntiJoin() {
    val r = for (k <- l if j forall (k2 => k !=# k2)) yield k
    val rAntiJoin = Optimization.cartProdToAntiJoin(r)
    //This was problematic because it transforms the newly-built Fun nodes into FunInterp ones, so we test that it works.
    val rOpt = optimize(rAntiJoin)
    println(r)
    println(rAntiJoin)
    println(rOpt)
    r.eval should equal (rAntiJoin.eval)
    r.eval should equal (rOpt.eval)
  }

  @Test
  def testTypeFilterPrimitive() {
    val base = asExp(Seq(1))
    val query1 = for (i <- base.typeFilter[Int] if i % 2 ==# 0) yield i
    val query2 = for (i <- base.typeFilter[Int] if i % 2 ==# 1) yield i
    query1.value() should be (Seq())
    query2.value() should be (Seq(1))
    val query21 = for (i <- base; iCast <- i.ifInstanceOf[Int] if iCast % 2 ==# 1) yield iCast
    query21.value() should be (Seq(1))
    val query21ExpectedOpt = for (i <- base.typeFilter[Int]; iCast <- Let(i) if iCast % 2 ==# 1) yield iCast
    Optimization.toTypeFilter(query21) should be (query21ExpectedOpt)
  }

  @Test
  def testTypeCase() {
    val exp = Seq(1, "foo", 5.0, new AnyRef).asSquopt typeCase (when[Int](_.toString_#), when[String](identity))
    println(exp)
    exp.eval.force should be (Set("1", "foo"))
    println(Compile toCode exp)
    Compile toValue exp should be (Set("1", "foo"))
  }

  @Test
  def testTypeCase2() {
    val exp = Seq(1, "foo", 5.0, new AnyRef).asSquopt typeCase (when[Int](_ => true, _ => "an int" /* + _.eval; _.toString*/), when[String](_ => true, identity))
    println(exp)
    exp.eval.force should be (Set("an int", "foo"))
    println(exp.eval.force)
    println(Compile toCode exp)
    Compile toValue exp should be (Set("an int", "foo"))
  }
}

