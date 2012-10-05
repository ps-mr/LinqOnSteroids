package ivm
package tests

import expressiontree.Lifting._
import expressiontree._
import optimization.Optimization
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import collection.TraversableView

class RemoveIdentityMapsTests extends JUnitSuite with ShouldMatchersForJUnit {
  type QRes = Exp[Traversable[Int]]
  val l: Exp[Traversable[Int]] = pure(Vector.range(1, 10))
  def withFilterQueries: (QRes, QRes) = {
    val q1 = for (
               x <- for (c <- l if c + 3 ==# 7; if c + 8 ==# 19) yield c
               if x ==# 19)
             yield x
    val q2 = Optimization.removeIdentityMaps(q1)
    (q1, q2)
  }

  @Test
  def testJoinOnWithFilter() {
    val (_, q2) = withFilterQueries
    //Compose the query, using the optimized version...
    val r = for (k <- q2; k2  <- l if k ==# k2) yield k + k2
    //Perform a further optimization
    val r2 = Optimization.optimizeCartProdToJoin(r)

    //Now this will fail with ClassCastException, if we built a Join based on a WithFilter.
    r2.interpret
  }

  @Test
  def testJoinOnWithFilter2() {
    val (q1, _) = withFilterQueries
    //First compose the query, based on the unoptimized version...
    val r = for (k <- q1; k2  <- l if k ==# k2) yield k + k2
    //Then do both optims
    val r2 = Optimization.optimizeCartProdToJoin(Optimization.removeIdentityMaps(r))

    //Now this might fail as above.
    r2.interpret
  }

  @Test
  def testRemoveIdentityMaps() {
    val (_, q2) = withFilterQueries

    val desiredResult: QRes = Filter(
                         Filter(
                           Filter(
                             l,
                             Fun((v24245:Exp[Int]) => Eq(Plus(v24245,3),7))),
                           Fun((v24246:Exp[Int]) => Eq(Plus(v24246,8),19))),
                        Fun((v24248:Exp[Int]) => Eq(v24248,19)))
    q2 should equal (desiredResult)

    // now merge the filters

    val q3 = Optimization.mergeFilters(q2)
    val finalResult: QRes = newWithFilter(
                             l,
                             Fun((v:Exp[Int]) => And(And(Eq(Plus(v,3),7), Eq(Plus(v,8),19)), Eq(v,19))))

    q3 should equal (finalResult)
  }
}
