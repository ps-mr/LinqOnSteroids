package ivm
package tests

import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test

class CallTests extends JUnitSuite with ShouldMatchersForJUnit {
  def test(x: Int, y: Int): Boolean = x+y == 12
  def foo(x: Int): Int = x
  def bar(x: Int): Int = x
  val l1 = fmap(2, 3)('test$Int$Int, test)
  val l2 = fmap(3, 4)('test$Int$Int, test)
  val l3 = fmap(2, 3)('test$Int$Int, test)
  val l4 = fmap(3)('bar$Int, bar)
  val l5 = fmap(3)('foo$Int, foo)
  val l6 = fmap(3)('foo$Int, bar) // deliberate violation of the liftCall contract

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
    Set[Any](l1,l2,l3,l4,l5,l6) should equal (Set[Any](l2,l3,l4,l5))
  }

  @Test
  def testWithExp() {
    //Normally use fmap instead - we want to show the problem with this definition.
    def withExp[T, U](t: Exp[T])(f: T => U): Exp[U] = asExp(f)(t)

    val a = withExp(1)(1 +)
    val b = withExp(1)(1 +)

    //a and b are semantically equal, but unfortunately they are not recognized as such, thus we cannot use such a simple
    //definition
    a should not equal (b)

    val c = fmap(1)('plusOne, 1 +)
    val d = fmap(1)('plusOne, 1 +)

    c should equal (d)
  }
}

