package ivm.expressiontree

import ref.WeakReference
import collection.mutable.{ArrayBuffer, HashMap, Subscriber, Publisher, Buffer, Set}

/**
 * Question: should we send updates before or after modifying the underlying collection? Here we do it before, so that
 * the subscribers can check the original collection value. Scala's interface does it afterwards, so that the update 
 * User: pgiarrusso
 * Date: 13/8/2011
 */

sealed trait Message[+T]
case class Include[T](t: T) extends Message[T]
case class Remove[T](t: T) extends Message[T]
case class Update[T](old: T, curr: T) extends Message[T]
case class Reset() extends Message[Nothing]

//Script nodes can be useful as a space optimization for Seq[Message[T]]; however, types become less perspicuous.
//case class Script[T](changes: Message[T]*) extends Message[T]

// Publisher hangs onto the listeners directly, while in our scenario weak references are badly needed.
// Here's a version of Publisher without this problem; the design is very similar to the original Publisher class, but
// with weak references and without filters (since we currently don't need them).

trait Publisher2[Evt] {
  type Pub <: Publisher2[Evt]
  type Sub = Subscriber[Evt, Pub]

  protected val self: Pub = this.asInstanceOf[Pub]
  //XXX: If Pub were a type parameter, then we could just write (I expect) self: Pub => at the beginning, instead of
  //such an ugly cast

  var subscribers: Seq[WeakReference[Sub]] = Seq()
  def subscribe(sub: Sub) {
    subscribers :+= new WeakReference(sub)
  }
  def removeSubscription(sub: Sub) {
    subscribers :+= new WeakReference(sub)
  }

  def publish(evt: Evt) {
    for (subWeakRef <- subscribers; sub <- subWeakRef.get) {
      sub.notify(self, evt)
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

  override def notify(pub: Repr, evts: Seq[Message[T]]) = publish(evts flatMap transformedMessages)
}

// All the maintainer classes/traits (MapMaintener, WithFilterMaintainer, FlatMapMaintainer) have a common structure of
// "message transformers". That can be probably abstracted away: they should have a method
// producedMessages: Message[T] => Seq[Message[T]], we should remove Script[T], implement publish in term of it in type:
// Forwarder[T, U, Repr] extends Subscriber[Seq[Message[T]], Repr] with Publisher[Seq[Message[U]]]
// notify(evts: Seq[Message[T]]) = publish(evts flatMap producedMessages)
// This should just be the default implementation though, because it doesn't use batching.
// Moreover, we might want to have by-name values there (impossible) or sth. alike - otherwise the pipeline will always
// execute the maintenance (say, apply the mapped function) before we get a chance to say "better not".
// Maybe we just bind the transformers together and pass them upwards together with the input, so that the concrete node
// decides whether to do the first application or not.
//
// Importantly, producedMessages can be recursive to reuse code - that's better than making notify recursive, because
// that forces to split notifications:
//case Update(old, curr) =>
//  notify(pub, Remove(old))
//  notify(pub, Include(curr))
//case Script(msgs @ _*) => msgs foreach (notify(pub, _))
//Moreover producedMessages is purely functional, unlike notify.
//
//XXX: mostly done up to here.
//
// Other point: this way, a pipeline of message transformers becomes simply a sequencing through >>= of monadic actions.
// Plus we can use MonadPlus.mplus for composing observables.




// This is for a modifiable sequence. Actually, it's a problem since we don't preserve ordering, but we should.
// On the one hand, this interface matches a relation in the closest way.
trait ObservableBuffer[T] extends Buffer[T] with MsgSeqPublisher[T] {
  type Pub <: ObservableBuffer[T]
  abstract override def clear() = {
    //publish(Script(this.map(Remove(_))))
    publish(Reset())
    super.clear()
  }
  abstract override def += (el: T) = {
    publish(Include(el))
    super.+=(el)
  }
  abstract override def +=: (el: T) = {
    publish(Include(el))
    super.+=:(el)
  }
  abstract override def update(n: Int, newelem: T) = {
    publish(Update(this(n), newelem))
    super.update(n, newelem)
  }
  abstract override def remove(n: Int) = {
    publish(Remove(this(n)))
    super.remove(n)
  }
  abstract override def insertAll(n: Int, iter: Traversable[T]) = {
    // Where we call toSeq is important - by creating the sequence once, we guarantee that the order of events is the
    // same as the actual insertion order.
    val seq = iter.toSeq
    publish(seq.map(Include(_)))
    super.insertAll(n, seq)
  }
}

trait QueryableBuffer[T, Repr] extends Queryable[T, Repr] with ObservableBuffer[T] {
  //We must propagate the bound on Traversable. While at it, we refine it.
  self: Buffer[T] with Repr =>
}

trait QueryableSet[T, Repr] extends Queryable[T, Repr] with ObservableSet[T] {
  //We must propagate the bound on Traversable. While at it, we refine it.
  self: Set[T] with Repr =>
}

// Here we don't get info about replaced elements. However, for observable elements, we should still register ourselves
// to forward them.
trait ObservableSet[T] extends Set[T] with MsgSeqPublisher[T] {
  type Pub <: ObservableSet[T]
  abstract override def clear() = {
    //publish(Script(this.map(Remove(_))))
    publish(Reset())
    super.clear()
  }
  abstract override def += (el: T) = {
    publish(Include(el))
    super.+=(el)
  }
  abstract override def -= (el: T) = {
    publish(Remove(el))
    super.-=(el)
  }
}

trait BufferObserver[T, U, Repr] extends MsgSeqSubscriber[T, Repr]/*QueryableBuffer[T, Repr]#Sub*/ with ObservableBuffer[U]

//case class MyBufferMap[T, U, Repr](f: T => U) extends ArrayBuffer[U] with BufferObserver[T, U, Repr]

// Let us first implement incremental view maintenance for multisets.
//Trait implementing incremental view maintenance for Map operations
trait MapMaintainer[T, U, Repr] extends EvtTransformer[T, U, Repr] {
  def fInt: T => U
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) => Seq(Include(fInt(v)))
      case Remove(v) => Seq(Remove(fInt(v)))
      case _ => defTransformedMessages(evt)

      /*case Reset() => Reset()
      //Here we implement an update by sending an update of the mapped element.
      //It's not clear whether this is any better. Moreover, what happens when we update a complex observable element?
      //If listeners also listen on the element itself, they are gonna get too many notifications.
      //Otherwise, they might ignore the notification from us... it's not clear.
      case Update(old, curr) => publish(Update(fInt(old), fInt(curr)))

      // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
      // Implement these in a superclass for these maintenance operations.
      //case Update(old, curr) => publish(Script(Remove(f(old)), Include(f(curr))))
      /*case Update(old, curr) =>
        notify(pub, Remove(old))
        notify(pub, Include(curr))*/
      case Script(msgs @ _*) => msgs foreach (notify(pub, _))*/
    }
  }
}

// Trait implementing incremental view maintenance for WithFilter operations.
// WithFilter clearly allows to represent the difference between two collections - however, such a WithFilter node
// can be converted to a difference node.
// OTOH, the semantics are the same for set difference, and slightly different but simpler to implement for multiset
// difference. Imagine c1@{a, a, b} - c2@{a} implemented with c1 withFilter (x => !(c2 exists (x == _))) or with
// multiset difference: we get {b} in the first case, {a, b} in the second.
// XXX: suppose we're using a bag for incremental view maintenance of a uniqueness constraint.
// Do we get a problem because of the different properties of subtraction?
trait WithFilterMaintainer[T, Repr] extends EvtTransformer[T, T, Repr] {
  def pInt: T => Boolean
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) => if (pInt(v)) Seq(Include(v)) else Seq.empty
      case Remove(v) => if (pInt(v)) Seq(Remove(v)) else Seq.empty
      case _ => defTransformedMessages(evt)
    }
  }
}

//Trait implementing incremental view maintenance for FlatMap operations.
//XXX: we don't listen yet to the individual collections! To subscribe, this needs to become a member of IncQueryReifier.
//Moreover, we need the passed function to return IncQueryReifiers.
trait FlatMapMaintainer[T, U, Repr] extends EvtTransformer[T, U, Repr] {
  self: IncQueryReifier[U] => //? [T]? That's needed for the subscribe.
  def fInt: T => QueryReifier[U] //XXX: QR |-> IncQR
  var cache = new HashMap[T, QueryReifier[U]] //XXX: QR |-> IncQR
  val subCollListener: MsgSeqSubscriber[U, IncQueryReifier[U]] = null
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) =>
        val fV = fInt(v)
        //fV subscribe subCollListener
        fV.exec().toSeq map (Include(_))
      case Remove(v) =>
        //val fV = fInt(v) //fV will not always return the _same_ result. We need a map from v to the returned collection. Damn!
        val fV = cache(v)
        //fV removeSubscription subCollListener
        fV.exec().toSeq map (Remove(_))
      case _ => defTransformedMessages(evt)
      /*case Reset() => publish(Reset())
      //Here we cannot implement an update by sending an update of the mapped element. But we should.
      //It's not clear whether this is any better. Moreover, what happens when we update a complex observable element?
      //If listeners also listen on the element itself, they are gonna get too many notifications.
      //Otherwise, they might ignore the notification from us... it's not clear.
      //case Update(old, curr) => publish(Update(f(old), f(curr)))
      // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
      //case Update(old, curr) => publish(Script(Remove(f(old)), Include(f(curr))))
      case Update(old, curr) =>
        notify(pub, Remove(old))
        notify(pub, Include(curr))
      case Script(msgs @ _*) => msgs foreach (notify(pub, _))*/
    }
  }
}

// TODO: add a trait which materializes update, and one which implements maintainance of unification.
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?

//Don't make Repr so specific as IncCollectionReifier. Making Repr any specific
//is entirely optional - it just enables the listener to get a more specific
//type for the pub param to notify(), if he cares.
class MapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,U]) extends Map[T,U](col, f) with MapMaintainer[T, U, IncQueryReifier[T]] {
  override def fInt = f.interpret()
}
class FlatMapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,/* XXX Inc */QueryReifier[U]]) extends FlatMap[T,U](col, f) with FlatMapMaintainer[T, U, IncQueryReifier[T]] with IncQueryReifier[U] {
  override def fInt = x => f.interpret()(x)
}
class WithFilterMaintainerExp[T](col: QueryReifier[T], p: FuncExp[T,Boolean]) extends WithFilter[T](col, p) with WithFilterMaintainer[T, IncQueryReifier[T]] {
  override def pInt = p.interpret()
}

// Variant of CollectionReifier, which also sends event to derived collections. Note that this is not reified!
// XXX: we forget to add mutation operations. But see Queryable and QueryableTest. So make this a trait which is mixed in
// by Queryable.
trait IncQueryReifier[T] extends QueryReifier[T] with MsgSeqPublisher[T] {
  type Pub <: IncQueryReifier[T]
  override def map[U](f: Exp[T] => Exp[U]): QueryReifier[U] = {
    val res = new MapMaintainerExp[T, U](this, FuncExp(f))
    this subscribe res
    res
    //val sub = new AnyRef with res.Sub /*MsgSeqSubscriber[U, Map[T, U]]*/ {
      //override def notify(pub: res.Pub, evt: Message[U]) {
    //val sub = new MsgSeqSubscriber[U, MsgSeqPublisher[U] /*MapMaintainerExp[T, U]*/] {
    //}
  }
  /*override def notify[U](pub: MapMaintainerExp[Message[T], ], evt: Message[T]) {
  }*/
  override def withFilter(p: Exp[T] => Exp[Boolean]): QueryReifier[T] = {
    val res = new WithFilterMaintainerExp[T](this, FuncExp(p))
    this subscribe res
    res
  }
  override def flatMap[U](f: Exp[T] => Exp[QueryReifier[U]]): QueryReifier[U] = {
    val res = new FlatMapMaintainerExp[T, U](this, FuncExp(f))
    this subscribe res
    res
  }
  //XXX add join, and add union
}


// The root of an incremental view maintenance chain.

//XXX: we might later want to inherit from ObservableSet and some specific set, instead of being an event forwarder.
// Especially because you must add elements to innercol, not to an instanc of this class, which is inconvenient!
class IncrementalSet[T](val innercol: ObservableSet[T]) extends IncQueryReifier[T] with ChildlessQueryReifier[T] with MsgSeqSubscriber[T, ObservableSet[T]] {
  type Pub <: IncrementalSet[T]
  innercol subscribe this
  override def exec(isLazy: Boolean) = if (isLazy) innercol.view else innercol
  override def notify(pub: ObservableSet[T], evts: Seq[Message[T]]) {
    publish(evts)
  }
}

//A class representing an intermediate or final result of an incremental query.
class IncrementalResult[T] extends ChildlessQueryReifier[T] with MsgSeqSubscriber[T, IncQueryReifier[T]] {
  var set = new HashMap[T, Int]
  override def exec(isLazy: Boolean) = set.keySet
  private[this] def count(v: T) = set.getOrElse(v, 0)
  override def notify(pub: IncQueryReifier[T], evts: Seq[Message[T]]) {
    for (evt <- evts) {
      evt match {
        case Include(v) => set(v) = count(v) + 1
        case Remove(v) =>
          val vCount = count(v) - 1
          if (vCount > 0)
            set(v) = vCount
          else
            set -= v
        case Reset() => set.clear()

        // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
        // The handling here is valid more in general, but no batching is done.
        case Update(old, curr) =>
          notify(pub, Seq(Remove(old), Include(curr)))
        //case Script(msgs @ _*) => msgs foreach (notify(pub, _))
      }
    }
  }
}
