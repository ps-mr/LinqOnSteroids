package ivm.expressiontree

import collection.mutable.{HashMap, Subscriber, Buffer, Set}

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
    // If we `Include` an element twice, we'll need to `Remove` it twice as well before e.g. the final IncrementalResult
    // instance realizes that it should disappear.
    if (!this(el))
      publish(Include(el))
    super.+=(el)
  }
  abstract override def -= (el: T) = {
    if (this(el))
      publish(Remove(el))
    super.-=(el)
  }
}

// Let us first implement incremental view maintenance for multisets.
//Trait implementing incremental view maintenance for Map operations
trait MapMaintainer[T, U, Repr] extends EvtTransformer[T, U, Repr] {
  def fInt: T => U
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) => Seq(Include(fInt(v)))
      case Remove(v) => Seq(Remove(fInt(v)))

      //Optionally, we can preserve update events as such.
      //case Update(old, curr) => publish(Update(fInt(old), fInt(curr)))
      //It's not clear whether this is any better. Moreover, what happens when we update a complex observable element?
      //If listeners also listen on the element itself, they are gonna get too many notifications.
      //Otherwise, they might ignore the notification from us... it's not clear.

      case _ => defTransformedMessages(evt)
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
  self: QueryReifier[U] => //? [T]? That's needed for the subscribe.
  def fInt: T => QueryReifier[U] //XXX: QR |-> IncQR
  var cache = new HashMap[T, QueryReifier[U]] //XXX: QR |-> IncQR
  val subCollListener: MsgSeqSubscriber[U, QueryReifier[U]] = null
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) =>
        val fV = fInt(v)
        //fV subscribe subCollListener //XXX reenable
        fV.exec().toSeq map (Include(_))
      case Remove(v) =>
        //val fV = fInt(v) //fV will not always return the _same_ result. We need a map from v to the returned collection. Damn!
        val fV = cache(v)
        //fV removeSubscription subCollListener //XXX reenable
        fV.exec().toSeq map (Remove(_))
      case _ => defTransformedMessages(evt)
      /*
      //Here we cannot implement an update by sending an update of the mapped element. But we should.
      //case Update(old, curr) => publish(Update(f(old), f(curr)))
      */
    }
  }
}

//Don't make Repr so specific as IncCollectionReifier. Making Repr any specific
//is entirely optional - it just enables the listener to get a more specific
//type for the pub param to notify(), if he cares.
class MapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,U]) extends Map[T,U](col, f)
with MapMaintainer[T, U, QueryReifier[T]] with QueryReifier[U] {
  override def fInt = f.interpret()
}
class FlatMapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,/* XXX Inc */QueryReifier[U]]) extends FlatMap[T,U](col, f)
with FlatMapMaintainer[T, U, QueryReifier[T]] with QueryReifier[U] {
  override def fInt = x => f.interpret()(x)
}
class WithFilterMaintainerExp[T](col: QueryReifier[T], p: FuncExp[T,Boolean]) extends WithFilter[T](col, p)
with WithFilterMaintainer[T, QueryReifier[T]] with QueryReifier[T] {
  override def pInt = p.interpret()
}

// Variant of QueryReifier, which also sends event to derived collections. Note that this is not reified!
// XXX: we forget to add mutation operations. But see Queryable and QueryableTest. So make this a trait which is mixed in
// by Queryable.
trait QueryReifier[T] extends QueryReifierBase[T] with MsgSeqPublisher[T] with Exp[QueryReifier[T]] {
  type Pub <: QueryReifier[T]
  override def map[U](f: Exp[T] => Exp[U]): QueryReifier[U] = {
    val res = new MapMaintainerExp[T, U](this, FuncExp(f))
    this subscribe res
    res
  }
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

// TODO: add a trait which implements maintenance of unification.
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?

//A class representing an intermediate or final result of an incremental query. Note: SetProxy is not entirely
// satisfactory - we want maybe something more like SetForwarder, which does not forward calls creating sequences of the
// same type. OTOH, this methods allows accessing the underlying data at all.
class IncrementalResult[T](val inner: QueryReifier[T]) extends ChildlessQueryReifier[T]
  with MsgSeqSubscriber[T, QueryReifier[T]]
  with collection.SetProxy[T] //I mean immutable.SetProxy[T], but that requires an underlying immutable Set.
  // I'll probably end up with forwarding most basic methods manually, and implementing the others through SetLike.
  // Or we'll just support incremental query update for all methods.
{
  var set = new HashMap[T, Int]
  inner subscribe this
  // It is crucial to have this statement only here after construction
  notify(inner, inner.exec().toSeq.map(Include(_)))

  //From SetProxy
  override def self = set.keySet

  override def exec() = self
  private[this] def count(v: T) = set.getOrElse(v, 0)
  override def notify(pub: QueryReifier[T], evts: Seq[Message[T]]) {
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
  override def toString = "IncrementalResult(" + exec().toString + ")"
}
