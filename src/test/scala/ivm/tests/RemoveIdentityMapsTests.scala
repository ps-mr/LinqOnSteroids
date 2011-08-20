package ivm
package tests

import optimization.Optimization
import collections.CollectionReifier
import ivm.{expressiontree, collections, optimization}
import collections.CollectionReifier
import expressiontree.Lifting._
import expressiontree.{And, FuncExp, Exp, Plus, Eq, WithFilter}
import optimization.Optimization
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test


class RemoveIdentityMapsTests extends JUnitSuite with ShouldMatchersForJUnit {
  val l: CollectionReifier[Int] = new CollectionReifier(Vector.range(1, 10))

  @Test
  def testRemoveIdentityMaps() {
    val q1 = for (
               x  : Exp[Int] <- for (c : Exp[Int] <- l if c + 3 is 7; if c + 8 is 19 ) yield c
               if x is 19)
             yield x
    val q2 = Optimization.removeIdentityMaps(q1)

    val desiredResult = WithFilter(
                         WithFilter(
                           WithFilter(
                             l,
                             FuncExp((v24245:Exp[Int]) => Eq(Plus(v24245,3),7))),
                           FuncExp((v24246:Exp[Int]) => Eq(Plus(v24246,8),19))),
                        FuncExp((v24248:Exp[Int]) => Eq(v24248,19)))
    assert(q2.potentiallyEquals(desiredResult))

    // now merge the filters

    val q3 = Optimization.mergeFilters(q2)
    val finalResult = WithFilter(
                             l,
                             FuncExp((v:Exp[Int]) => And(And(Eq(Plus(v,3),7), Eq(Plus(v,8),19)), Eq(v,19))))

    assert(q3.potentiallyEquals(finalResult))
  }
}