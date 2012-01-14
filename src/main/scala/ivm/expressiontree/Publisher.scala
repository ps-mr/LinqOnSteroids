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
/*class Include[T](_t: => T) extends Message[T] {
  lazy val t = _t
}*/
case class Remove[+T](t: T) extends TravMessage[T]
case class Update[+T](old: T, curr: T) extends TravMessage[T]
case object Reset extends TravMessage[Nothing]

//Allow updating single elements (e.g. the result of folds)!
case class NewVal[T](newV: T) extends Message[T]
case class UpdateVal[T](oldV: T, newV: T) extends Message[T]

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

trait Publisher[+Evt, +Pub <: Publisher[Evt, Pub]] {
  type Sub = Subscriber[Evt, Pub]

  def addSubscriber(sub: Sub)
  def removeSubscriber(sub: Sub)
  protected[this] def publish(evt: Evt)
}

trait IgnoringPublisher[+Evt, +Pub <: Publisher[Evt, Pub]] extends Publisher[Evt, Pub] {
  def addSubscriber(sub: Sub) {}
  def removeSubscriber(sub: Sub) {}
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
trait DefaultPublisher[+Evt, +Pub <: DefaultPublisher[Evt, Pub]] extends Publisher[Evt, Pub] {
  selfAsPub: Pub =>

  private[this] var subscribers: Set[EqWeakReference[Sub]] = HashSet.empty
  def addSubscriber(sub: Sub) {
    subscribers += new EqWeakReference(sub)
  }
  def removeSubscriber(sub: Sub) {
    subscribers -= new EqWeakReference(sub)
  }

  protected[this] def publish(evt: Evt) {
    for (subWeakRef <- subscribers; sub <- subWeakRef.get) {
      sub.notify(selfAsPub, evt)
    }
    subscribers = subscribers.filter(_.get.isDefined)
  }
}

//
trait MsgSeqPublisher[+T, +Pub <: MsgSeqPublisher[T, Pub]] extends
IgnoringPublisher[Seq[Message[T]], Pub]
//DefaultPublisher[Seq[Message[T]], Pub] //XXX: restore this after deadline
{
  selfAsPub: Pub =>
  protected[this] def publish(evt: Message[T]) {
    publish(Seq(evt))
  }
}

trait MsgSeqSubscriber[-T, -Repr] extends Subscriber[Seq[Message[T]], Repr]

trait EvtTransformerBase[-T, +U, -Repr] extends MsgSeqSubscriber[T, Repr] with MsgSeqPublisher[U, EvtTransformerBase[T, U, Repr]] {
  //Contract: transforms messages, potentially executes them.
  def transformedMessages(pub: Repr, v: Message[T]): Seq[Message[U]]

  override def notify(pub: Repr, evts: Seq[Message[T]]) {
    publish(evts flatMap (transformedMessages(pub, _)))
  }
}

trait EvtTransformer[-T, +U, -Repr] extends EvtTransformerBase[Traversable[T], Traversable[U], Repr] {
  //Precondition: only ever pass Reset or Update nodes.
  protected def defTransformedMessages(pub: Repr, v: TravMessage[T]): Seq[TravMessage[U]] = {
    v match {
      case Reset => Seq(Reset)
      case Update(old, curr) =>
        Seq(Remove(old), Include(curr)) flatMap (transformedMessages(pub, _))
      case _ =>
        throw new IllegalArgumentException("Unexpected message in defTransformedMessages")
      /*case Script(msgs @ _*) =>
        msgs flatMap transformedMessages*/
      //case v => producedMessages(v) //typechecks, makes the match exhaustive, causes a loop at runtime.
    }
  }

}

//Interface used by EvtTransformerEl
trait ExpWithCache[+T] extends Exp[T] {
  protected[this] def cache: Option[T]
}

// Cache the computed result value of an expression.
// One reusable implementation of the EvtTransformerEl interface
trait CachingExp[+T] extends ExpWithCache[T] {
  //This field is never set by Exp or CachingExp itself, only by result-caching nodes

  //Note: this declaration overrides both getter and setter. Therefore, they both need to be already declared in parent
  //types; therefore, we need to declare WorkaroundExp.
  protected[this] var cache: Option[T] = None

  override def expResult(): T = cache match {
    case None =>
      val res = interpret()
      cache = Some(res)
      res
    case Some(v) => v
  }
}

trait EvtTransformerEl[-T, +U, -Repr] extends MsgSeqSubscriber[T, Repr] with MsgSeqPublisher[U, Exp[U]] {
  this: ExpWithCache[U] =>

  def notifyEv(pub: Repr, evt: Message[T])
  override def notify(pub: Repr, evts: Seq[Message[T]]) {
    val oldRes = cache
    evts foreach (notifyEv(pub, _))
    assert(cache.isDefined)
    oldRes match {
      case Some(oldVal) =>
        publish(UpdateVal(oldVal, cache.get))
      case None =>
        publish(NewVal(cache.get))
    }
  }
}

object Debug {
  val verbose = false
}

//XXX: make EvtTransformerEl descend from EvtTransformerBase

// vim: set ts=4 sw=4 et:
