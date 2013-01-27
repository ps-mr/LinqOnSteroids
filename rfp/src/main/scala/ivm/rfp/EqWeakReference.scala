package ivm.rfp

import ref.WeakReference

/**
 * User: pgiarrusso
 * Date: 13/8/2011
 */

/**
 * Extends WeakReference with working equality comparison
 */
class EqWeakReference[+T >: Null <: AnyRef](t: T) extends WeakReference[T](t: T) with Equals {
  override def canEqual(that: Any) = that.isInstanceOf[EqWeakReference[_]]
  private def getOrNull[S >: Null <: AnyRef](x: WeakReference[S]): S = x.get.orNull
  override def equals(other: Any) =
    other match {
      case that: AnyRef if that eq this => true
      case that: EqWeakReference[_] =>
        (that canEqual this) &&
        //XXX: use eq or equals? equals makes more sense in general, but eq makes more sense for our use case.
        (getOrNull(this) eq getOrNull(that))
      case _ =>
        false
      // There is a definition coming from Proxy; however Proxy is used around java.lang.ref.WeakReference, which
      // uses identity comparison.
      //super.equals(other)
    }
  override def hashCode() = System.identityHashCode(getOrNull(this)) //Use identityHashCode(_) instead of _.hashCode() to get a constant hashcode;
  // but call it on the actual object, not on some Option wrapper.
}

