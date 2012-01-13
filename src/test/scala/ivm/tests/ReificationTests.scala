package ivm
package tests

import scala.collection.immutable.Vector
import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import optimization.Optimization
import Optimization.optimize

class ReificationTests extends JUnitSuite with ShouldMatchersForJUnit {
  def test(x: Int, y: Int): Boolean = x+y == 12
  def foo(x: Int): Int = x
  def bar(x: Int): Int = x
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1,10))
  val j: Exp[Traversable[Int]] = toExp(Vector.range(1,10))
  val q = for (k  <- l if k <= 5) yield 3 + k
  val r = for (k  <- l; k2  <- j if k is k2) yield k + k2
  val r1 = for (k <- l; k2 <- j if k + k2 is k2 + k) yield k + k2
  val r2 = for (k <- l; k2 <- j if liftCall('test$Int$Int, test, k, k2)) yield k + k2
  val r3 = for (k <- l; k2 <- j if liftCall('foo$Int, foo, k) is liftCall('bar$Int, bar, k2)) yield (k, k2)
  
  @Test
  def testq() {
    optimize(q) should equal (q)
  }
  @Test
  def testr() {
    optimize(r) should equal (l.join(j)(x => x, y => y, p => (p._1 + p._2)))
  }

  @Test
  def testr1() {
    optimize(r1) should equal (r1)
  }
  
  @Test
  def testr2() {
    optimize(r2) should equal (r2)
  }
  
  @Test
  def testr3() {
    optimize(r3) should equal (l.join(j)(x => liftCall('foo$Int,foo,x), y => liftCall('bar$Int, bar, y), p => (p._1, p._2)))
  }
  
  @Test 
  def testfreeVars() {
    q.freeVars should be (Set.empty)
    newMapOp(l,FuncExp( (x: Exp[Int]) => x + Var(58))).freeVars should be (Set(Var(58)))
  }

  @Test
  def testReification() {
    q should equal (newMapOp[Int, Traversable[Int], Int, Traversable[Int]](newWithFilter(l, FuncExp((v2: Exp[Int]) => LEq(v2, 5))),FuncExp((v3: Exp[Int]) => Plus(3, v3))))
    /*println(q.exec())
    println(optimize(q).interpret().exec())
    //val r = l.flatMap( (k) => j.withFilter( (k2 ) => k is k2).map( (k2:Exp[Int]) => k+k2))
    println(r);
    println(optimize(r))
    println(r.exec())
    println(optimize(r).interpret().exec())
    println(r1);
    println(optimize(r1))
    println(r.exec())
    println(r2);
    println(optimize(r2))
    println(r.exec())
    println(r3);
    println(optimize(r3))
    println(r3.exec())
    println(optimize(r3).interpret().exec())*/
  }
  
  @Test
  def testAntiJoin() {
    val r = for (k <- l if j forall (k2 => k !== k2)) yield k
    val rAntiJoin = Optimization.cartProdToAntiJoin(r)
    //This was problematic because it transforms the newly-built FuncExp nodes into FuncExpInt ones, so we test that it works.
    val rOpt = optimize(rAntiJoin)
    println(r)
    println(rAntiJoin)
    println(rOpt)
    r.interpret() should equal (rAntiJoin.interpret())
    r.interpret() should equal (rOpt.interpret())
  }

  @Test
  def testTypeFilterPrimitive() {
    val base = asExp(Seq(1))
    val query1 = for (i <- base.typeFilter[Int] if i % 2 === 0) yield i
    val query2 = for (i <- base.typeFilter[Int] if i % 2 === 1) yield i
    query1.expResult() should be (Seq())
    query2.expResult() should be (Seq(1))
    val query21 = for (i <- base; iCast <- i.ifInstanceOf[Int] if iCast % 2 === 1) yield iCast
    query21.expResult() should be (Seq(1))
    val query21ExpectedOpt = for (i <- base.typeFilter[Int]; iCast <- Let(i) if iCast % 2 === 1) yield iCast
    Optimization.toTypeFilter(query21) should be (query21ExpectedOpt)
  }
}

