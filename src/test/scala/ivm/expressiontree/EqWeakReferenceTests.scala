package ivm.expressiontree

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import ref.WeakReference

/**
* User: pgiarrusso
* Date: 15/9/2011
*/

class EqWeakReferenceTests extends JUnitSuite with ShouldMatchersForJUnit {
  @Test
  def equals() {
    val o = new AnyRef()
    val a = new EqWeakReference(o)
    val b = new EqWeakReference(o)
    a should (be === b)
    val nullRef = new EqWeakReference(null)
    a should (not (be === nullRef))
    nullRef should (not (be === a))
    val d = new EqWeakReference(null)
    nullRef should (be === d)
    val e = new WeakReference(o)
    a should (not (be === e))
    e should (not (be === a))
  }
}
