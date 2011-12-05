package ivm.expressiontree

import collection.immutable.HashSet
import collection.mutable.Subscriber

/**
 * User: pgiarrusso
 * Date: 5/12/2011
 */
trait Publisher2[+Evt, +Pub <: Publisher2[Evt, Pub]] {
  type Sub = Subscriber[Evt, Pub]

  def subscribe(sub: Sub)
  def removeSubscription(sub: Sub)
  protected[this] def publish(evt: Evt)
}

trait DefaultPublisher2[+Evt, +Pub <: DefaultPublisher2[Evt, Pub]] extends Publisher2[Evt, Pub] {
  selfAsPub: Pub =>

  private[this] var subscribers: Set[EqWeakReference[Sub]] = HashSet.empty
  def subscribe(sub: Sub) {
    subscribers += new EqWeakReference(sub)
  }
  def removeSubscription(sub: Sub) {
    subscribers -= new EqWeakReference(sub)
  }

  protected[this] def publish(evt: Evt) {
    for (subWeakRef <- subscribers; sub <- subWeakRef.get) {
      sub.notify(selfAsPub, evt)
    }
    subscribers = subscribers.filter(_.get.isDefined)
  }
}
