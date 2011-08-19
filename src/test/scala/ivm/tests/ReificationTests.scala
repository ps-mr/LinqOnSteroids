package ivm
package tests

import scala.collection.immutable.Vector
import optimization.Optimization._
import collections.CollectionReifier
import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class ReificationTests extends JUnitSuite with ShouldMatchersForJUnit  {
  def test(x: Int, y: Int) : Boolean = x+y == 12
  def foo(x: Int) : Int = x
  def bar(x: Int) : Int = x
  val l : QueryReifier[Int] = new CollectionReifier(Vector.range(1,10))
  val j : QueryReifier[Int] = new CollectionReifier(Vector.range(1,10))
  val q = for (k  <- l if k <= 5 ) yield k+3
  val r = for (k  <- l; k2  <- j if k is k2) yield k+k2
  val r1 = for (k <- l; k2 <- j if k + k2 is k2 + k) yield k+k2
  val r2 = for (k <- l; k2 <- j if liftCall(test, k,k2)) yield k+k2
  val r3 = for (k <- l; k2 <- j if liftCall(foo,k) is liftCall(bar,k2)) yield (k,k2)
  @Test
  def testq() {
    optimize(q) should be (q)
  }
  @Test
  def testr() {
    assert(optimize(r).potentiallyEquals( (l.join(j, (x) => x, (y: Exp[Int]) => y, (p: Exp[(Int,Int)]) => (p._1 + p._2)))))
  }

  @Test
  def testr1() {
    optimize(r1) should be (r1)
  }
  
  @Test
  def testr2() {
    assert(optimize(r2).potentiallyEquals(r2))
  }
  
  @Test
  def testr3() {
    assert(optimize(r3).potentiallyEquals( (l.join(j,(x) => liftCall(foo,x),(y:Exp[Int]) => liftCall(bar,y),(p: Exp[(Int,Int)]) => (p._1,p._2)))))
  }
  
  @Test 
  def testfreeVars() {
    q.freeVars should be (Set.empty)
    Map(l,FuncExp( (x: Exp[Int]) => x + Var("y"))).freeVars should be (Set(Var("y")))
  }

  @Test
  def testReification() {
    q should be (Map(WithFilter(l,FuncExp((v2 : Exp[Int]) => LEq(v2,5))),FuncExp((v3: Exp[Int]) => Plus(v3,3))))
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
  def performancetests() {
    import Benchmarking._
    //small performance test
    val testSize = 2000
    val c1 : QueryReifier[Int] = new CollectionReifier(Vector.range(1,testSize))
    val c2 : QueryReifier[Int] = new CollectionReifier(Vector.range(1,testSize))
    val n1 = Vector.range(1,testSize)
    val n2 = Vector.range(1,testSize)
    val notoptimized = for (k  <- c1; k2  <- c2 if k is k2) yield k+k2
    silentBenchMark("Native query") {
      val nativequery  = for (k  <- n1; k2  <- n2 if k.equals(k2)) yield k+k2
    }
    silentBenchMark("Native query with primitive ==") {
      // The native query is significantly faster if we compare by  k == k2 instead of k.equals(k2)
      val nativequery  = for (k  <- n1; k2  <- n2 if k == k2) yield k+k2
    }

    // Surprisingly, the notoptimized query becomes much slower if we implement 
    // Eq.compare with == instead of equals. Klaus
    // RE: In my tests, I seldom get a difference, it is not significant, and seems to disappear
    // by enabling the Scala optimizer.
    assert(
      silentBenchMark("Non optimized")(notoptimized.exec().size)
      > (2 * silentBenchMark("Optimized")(optimize(notoptimized).interpret().exec().size)))
  }

 
}

