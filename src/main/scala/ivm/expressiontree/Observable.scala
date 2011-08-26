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

//A class representing an intermediate or final result of an incremental query. Note: SetProxy is not entirely
// satisfactory - we want maybe something more like SetForwarder, which does not forward calls creating sequences of the
// same type. OTOH, this methods allows accessing the underlying data at all.
class IncrementalResult[T](val inner: QueryReifier[T]) extends ChildlessQueryReifier[T]
  with MsgSeqSubscriber[T, QueryReifier[T]]
  with Queryable[T, collection.SetProxy[T]]
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
  override def toString = "IncrementalResult(" + self.toString + ")"
}
