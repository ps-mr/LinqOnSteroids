package ivm.expressiontree

import ref.WeakReference
import collection.mutable.Subscriber
import collection.immutable.HashSet

/**
 * Question: should we send updates before or after modifying the underlying collection? Here we do it before, so that
 * the subscribers can check the original collection value. Scala's interface does it afterwards, so that the update 
 * User: pgiarrusso
 * Date: 13/8/2011
 */

sealed trait Message[+T]
case class Include[+T](t: T) extends TravMessage[T]
/*class Include[T](_t : => T) extends Message[T] {
  lazy val t = _t
}*/
case class Remove[+T](t: T) extends TravMessage[T]
case class Update[+T](old: T, curr: T) extends TravMessage[T]
case object Reset extends TravMessage[Nothing]

//Allow updating single elements (e.g. the result of folds)!
case class UpdateEl[T](oldV: T, newV: T) extends Message[T]

// XXX: A union class will have a hard time handling a Reset event. Maybe it's better to just batch Remove messages for
// a Reset? That's a problem when reset is O(1); maybe that must be done by intermediate nodes, which don't have however
// the elements anyway, because they have not been transformed.

//Script nodes can be useful as a space optimization for Seq[Message[T]]; however, types become less perspicuous.
//case class Script[T](changes: Message[T]*) extends Message[T]

// DefaultPublisher hangs onto the listeners directly, while in our scenario weak references are badly needed.
// Here's a version of DefaultPublisher without this problem; the design is very similar to the original DefaultPublisher class, but
// with weak references and without filters (since we currently don't need them).

/* TODO Copyright: our DefaultPublisher class derives from Publisher in the Scala library, which has a BSD license.
 * Copying code is allowed and no problem, as long as we acknowledge it in the sources. */

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

trait Publisher[+Evt] {
  type Pub <: Publisher[Evt]
  type Sub = Subscriber[Evt, Pub]

  def subscribe(sub: Sub)
  def removeSubscription(sub: Sub)
  protected[this] def publish(evt: Evt)
}

trait IgnoringPublisher[+Evt] extends Publisher[Evt] {
  def subscribe(sub: Sub) {}
  def removeSubscription(sub: Sub) {}
  protected[this] def publish(evt: Evt) {}
}

/*
 * DefaultPublisher hangs onto subscribers through weak references. The subscribers, instead, hang onto the nodes they
 * listen to through strong references. This way, whenever a query result is thrown away, the intermediate nodes which
 * were needed for it can be garbage collected.
 * If the original collection is no more referenced elsewhere, the query results will keep it in scope. However, if it
 * cannot be modified now, maybe one should allow it to be GC'ed? Not in general, because that would prevent reevaluation
 * of the results. TODO: We could introduce an IncrementalResult.detach() method for these situations.
 * The alternative would be that the intermediate nodes keep only a weak reference to the base collections - which is
 * a special node anyway (it must be an incremental collection, like IncArrayBuffer or IncHashSet).
 * Moreover, also self-maintainable nodes could hang onto their sources through weak references.
 * But XXX: We don't have (yet) a concept of Self-Maintainable View (which exists in databases to save IO).
 */
trait DefaultPublisher[+Evt] extends Publisher[Evt] {
  type Pub <: DefaultPublisher[Evt]

  protected def selfAsPub: Pub = this.asInstanceOf[Pub]
  //XXX: If Pub were a (covariant) type parameter, then we could just write (I expect) selfAsPub: Pub => at the beginning, instead of
  //such an ugly cast. However, Pub appears in Subscriber in a contravariant position, so that's not so easily possible.

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

trait MsgSeqPublisher[+T] extends DefaultPublisher[Seq[Message[T]]] {
  protected[this] def publish(evt: Message[T]) {
    publish(Seq(evt))
  }
}

trait MsgSeqSubscriber[-T, -Repr] extends Subscriber[Seq[Message[T]], Repr]

trait EvtTransformer[-T, +U, -Repr] extends TravMsgSeqSubscriber[T, Repr] with TravMsgSeqPublisher[U] {
  //Contract: transforms messages, potentially executes them.
  def transformedMessages(v: TravMessage[T]): Seq[TravMessage[U]]

  //Precondition: only ever pass Reset or Update nodes.
  protected def defTransformedMessages(v: TravMessage[T]): Seq[TravMessage[U]] = {
    v match {
      case Reset => Seq(Reset)
      case Update(old, curr) =>
        Seq(Remove(old), Include(curr)) flatMap transformedMessages
      case _ =>
        throw new IllegalArgumentException("Unexpected message in defTransformedMessages")
      /*case Script(msgs @ _*) =>
        msgs flatMap transformedMessages*/
      //case v => producedMessages(v) //typechecks, makes the match exhaustive, causes a loop at runtime.
    }
  }

  override def notify(pub: Repr, evts: Seq[TravMessage[T]]) = {
    val res = evts flatMap transformedMessages
    if (Debug.verbose)
      println("%s notify(\n  pub = %s,\n  evts = %s\n) = %s" format (this, pub, evts, res))
    publish(res)
  }
}

object Debug {
  val verbose = false
}
// vim: set ts=4 sw=4 et:
