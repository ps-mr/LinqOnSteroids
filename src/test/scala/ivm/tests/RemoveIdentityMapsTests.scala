package ivm
package tests

import expressiontree.Lifting._
import expressiontree.{And, FuncExp, Exp, Plus, Eq, View, Filter}
import optimization.Optimization
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import collection.TraversableView

class RemoveIdentityMapsTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: Exp[Traversable[Int]] = toExp(Vector.range(1, 10))
  def withFilterQueries = {
    val q1 = for (
               x <- for (c <- l if c + 3 is 7; if c + 8 is 19) yield c
               if x is 19)
             yield x
    val q2 = Optimization.removeIdentityMaps(q1)
    (q1, q2)
  }

  @Test
  def testJoinOnWithFilter() {
    val (_, q2) = withFilterQueries
    //Compose the query, using the optimized version...
    val r = for (k <- q2; k2  <- l if k is k2) yield k + k2
    //Perform a further optimization
    val r2 = Optimization.optimizeCartProdToJoin(r)

    //Now this will fail with ClassCastException, if we built a Join based on a WithFilter.
    r2.interpret()
  }

  @Test
  def testJoinOnWithFilter2() {
    val (q1, _) = withFilterQueries
    //First compose the query, based on the unoptimized version...
    val r = for (k <- q1; k2  <- l if k is k2) yield k + k2
    //Then do both optims
    val r2 = Optimization.optimizeCartProdToJoin(Optimization.removeIdentityMaps(r))

    //Now this might fail as above.
    r2.interpret()
  }

  @Test
  def testRemoveIdentityMaps() {
    val (_, q2) = withFilterQueries

    val desiredResult = Filter(
                         Filter(
                           Filter(
                             View[Int, Traversable[Int]](l),
                             FuncExp((v24245:Exp[Int]) => Eq(Plus(v24245,3),7))),
                           FuncExp((v24246:Exp[Int]) => Eq(Plus(v24246,8),19))),
                        FuncExp((v24248:Exp[Int]) => Eq(v24248,19)))
    q2 should equal (desiredResult)

    // now merge the filters

    val q3 = Optimization.mergeFilters(q2)
    val finalResult = newWithFilter(
                             l,
                             FuncExp((v:Exp[Int]) => And(And(Eq(Plus(v,3),7), Eq(Plus(v,8),19)), Eq(v,19))))

    q3 should equal (finalResult)
  }
}
