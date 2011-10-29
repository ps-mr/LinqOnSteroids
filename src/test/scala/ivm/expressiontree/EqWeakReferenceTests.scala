package ivm.expressiontree

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import ref.WeakReference

/**
* User: pgiarrusso
* Date: 15/9/2011
*/

class EqWeakReferenceTests extends JUnitSuite with ShouldMatchersForJUnit {
  def compare[T >: Null <: AnyRef](l: EqWeakReference[T], r: EqWeakReference[T]) {
    l should (be === r)
    //l should (equal(r))
    l.hashCode() should (equal(r.hashCode()))
  }
  @Test
  def equals() {
    val o = new AnyRef()
    val a = new EqWeakReference(o)
    val b = new EqWeakReference(o)
    compare(a, b)
    val nullRef = new EqWeakReference(null)
    a should (not (be === nullRef))
    nullRef should (not (be === a))
    val d = new EqWeakReference(null)
    compare(nullRef, d)
    val e = new WeakReference(o)
    a should (not (be === e))
    e should (not (be === a))
  }
}
