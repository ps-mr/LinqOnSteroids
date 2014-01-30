package ivm.expressiontree

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.Test
import ref.WeakReference
import ivm.rfp.EqWeakReference

/**
* User: pgiarrusso
* Date: 15/9/2011
*/

class EqWeakReferenceTests extends JUnitSuite with Matchers with AssertionsForJUnit {
  //Should be part of ScalaTest.
  def assertEqualsAndSameHash[T](l: T, r: T) {
    l should === (r)
    l.hashCode should === (r.hashCode)
  }

  val o = new AnyRef()
  @Test
  def equals() {
    val refO1 = new EqWeakReference(o)
    val refO2 = new EqWeakReference(o)
    assertEqualsAndSameHash(refO1, refO2)

    val nullRef1 = new EqWeakReference(null)
    refO1 should !== (nullRef1)
    nullRef1 should !== (refO1)

    val nullRef2 = new EqWeakReference(null)
    assertEqualsAndSameHash(nullRef1, nullRef2)
  }

  @Test
  def eqWeakReferenceDiffersFromWeakReference() {
    val eqWeakRef = new EqWeakReference(o)
    val weakRef = new WeakReference(o) //Note that this is WeakReference, not EqWeakReference, and thus differs from a.
    eqWeakRef should !== (weakRef)
    weakRef should !== (eqWeakRef)
  }
}
