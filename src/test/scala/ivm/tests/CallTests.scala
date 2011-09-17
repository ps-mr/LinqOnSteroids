package ivm
package tests

import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class CallTests extends JUnitSuite with ShouldMatchersForJUnit  {
  def test(x: Int, y: Int) : Boolean = x+y == 12
  def foo(x: Int) : Int = x
  def bar(x: Int) : Int = x
  val l1 = liftCall('test$Int$Int,test, 2,3)
  val l2 = liftCall('test$Int$Int,test, 3,4)
  val l3 = liftCall('test$Int$Int,test, 2,3)
  val l4 = liftCall('bar$Int,bar, 3)
  val l5 = liftCall('foo$Int,foo, 3)
  val l6 = liftCall('foo$Int,bar, 3) // deliberate violation of the liftCall contract

  @Test
  def testCalls() {
    l1 should equal (l3)
    l3 should equal (l1)
    l1 should not equal (l2)
    l2 should not equal (l1)
    l4 should not equal (l5)
    l5 should not equal (l4)
    l6 should equal (l5)
    l5 should equal (l6)
    Set(l1,l2,l3,l4,l5,l6) should equal (Set(l2,l3,l4,l5))

  }

}

