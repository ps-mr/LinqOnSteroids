package ivm.expressiontree

import ref.WeakReference
import collection.mutable.Subscriber

/**
 * Question: should we send updates before or after modifying the underlying collection? Here we do it before, so that
 * the subscribers can check the original collection value. Scala's interface does it afterwards, so that the update 
 * User: pgiarrusso
 * Date: 13/8/2011
 */

sealed trait Message[+T]
case class Include[T](t: T) extends Message[T]
/*class Include[T](_t : => T) extends Message[T] {
  lazy val t = _t
}*/
case class Remove[T](t: T) extends Message[T]
case class Update[T](old: T, curr: T) extends Message[T]
case class Reset() extends Message[Nothing]
// XXX: A union class will have a hard time handling a Reset event. Maybe it's better to just batch Remove messages for
// a Reset? That's a problem when reset is O(1); maybe that must be done by intermediate nodes, which don't have however
// the elements anyway, because they have not been transformed.

//XXX: We don't have (yet) a concept of Self-Maintenable View, which in databases saves IO and for us can allow
// garbage-collecting the original collection. But they are problematic. Here's the problem and a potential solution.
// Publishers hang onto their subscribers through weak references, hence each subscriber must keep the
// previous one alive. We can allow only the original collection to be GC-ed, not the intermediate message transformers.
// Hence the first-level intermediate nodes must use a weak reference to the original collection; other nodes have just
// a strong reference.

//Script nodes can be useful as a space optimization for Seq[Message[T]]; however, types become less perspicuous.
//case class Script[T](changes: Message[T]*) extends Message[T]

// Publisher hangs onto the listeners directly, while in our scenario weak references are badly needed.
// Here's a version of Publisher without this problem; the design is very similar to the original Publisher class, but
// with weak references and without filters (since we currently don't need them).

/* TODO Copyright: our Publisher class derives from the Scala library, which has a BSD license. Copying code is allowed
 * and no problem, as long as we acknowledge it in the sources. */
trait Publisher[Evt] {
  type Pub <: Publisher[Evt]
  type Sub = Subscriber[Evt, Pub]

  protected def selfAsPub: Pub = this.asInstanceOf[Pub]
  //XXX: If Pub were a type parameter, then we could just write (I expect) selfAsPub: Pub => at the beginning, instead of
  //such an ugly cast

  var subscribers: Seq[WeakReference[Sub]] = Seq()
  def subscribe(sub: Sub) {
    subscribers :+= new WeakReference(sub)
  }
  def removeSubscription(sub: Sub) {
    subscribers = subscribers filter (_.get == sub)
  }

  def publish(evt: Evt) {
    for (subWeakRef <- subscribers; sub <- subWeakRef.get) {
      sub.notify(selfAsPub, evt)
    }
    subscribers = subscribers.filter(_.get.isDefined)
  }
}

trait MsgSeqPublisher[T] extends Publisher[Seq[Message[T]]] {
  def publish(evt: Message[T]) {
    publish(Seq(evt))
  }
}

trait MsgSeqSubscriber[-T, -Repr] extends Subscriber[Seq[Message[T]], Repr]

trait EvtTransformer[-T, U, -Repr] extends MsgSeqSubscriber[T, Repr] with MsgSeqPublisher[U] {
  //Contract: transforms messages, potentially executes them.
  def transformedMessages(v: Message[T]): Seq[Message[U]]

  //Precondition: only ever pass Reset or Update nodes.
  protected def defTransformedMessages(v: Message[T]): Seq[Message[U]] = {
    v match {
      case Reset() => Seq(Reset())
      case Update(old, curr) =>
        Seq(Remove(old), Include(curr)) flatMap transformedMessages
      case _ =>
        throw new IllegalArgumentException("Unexpected message in defTransformedMessages")
      /*case Script(msgs @ _*) =>
        msgs flatMap transformedMessages*/
      //case v => producedMessages(v) //typechecks, makes the match exhaustive, causes a loop at runtime.
    }
  }

  override def notify(pub: Repr, evts: Seq[Message[T]]) = {
    val res = evts flatMap transformedMessages
    if (Debug.verbose)
      println("%s notify(\n  pub = %s,\n  evts = %s\n) = %s" format (this, pub, evts, res))
    publish(res)
  }
}

object Debug {
  val verbose = true
}
// vim: set ts=4 sw=4 et:
