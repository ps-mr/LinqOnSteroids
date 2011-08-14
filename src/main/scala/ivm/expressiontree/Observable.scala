package ivm.expressiontree

import collection.mutable.{Subscriber, Publisher, Buffer, ArrayBuffer, Set}

//import collection.mutable._

/**
 * Question: should we send updates before or after modifying the underlying collection? Here we do it before, so that
 * the subscribers can check the original collection value. Scala's interface does it afterwards. On the o
 * User: pgiarrusso
 * Date: 13/8/2011
 */

sealed trait Message[T]
case class Include[T](t: T) extends Message[T]
case class Remove[T](t: T) extends Message[T]
case class Update[T](old: T, curr: T) extends Message[T]
case class Reset[T]() extends Message[T]
case class Script[T](changes: Message[T]*) extends Message[T]

// This is for a modifiable sequence. Actually, it's a problem since we don't preserve ordering, but we should.
// On the one hand, this interface matches a relation in the closest way.
trait ObservableBuffer[T] extends Buffer[T] with Publisher[Message[T]] {
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
    publish(Script(seq.map(Include(_)): _*))
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
trait ObservableSet[T] extends Set[T] with Publisher[Message[T]] {
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

trait BufferObserver[T, U, Repr] extends Subscriber[Message[T], Repr]/*QueryableBuffer[T, Repr]#Sub*/ with ObservableBuffer[U]

//case class MyBufferMap[T, U, Repr](f: T => U) extends ArrayBuffer[U] with BufferObserver[T, U, Repr]

//Trait implementing incremental view maintenance for Map operations.
case class MapMaintainer[T, U, Repr](f: T => U) extends Subscriber[Message[T], Repr] with Publisher[Message[U]] {
  override def notify(pub: Repr, evt: Message[T]) {
    evt match {
      case Include(v) => publish(Include(f(v)))
      case Remove(v) => publish(Remove(f(v)))
      case Reset() => publish(Reset())
      //Here we implement an update by sending an update of the mapped element.
      //It's not clear whether this is any better. Moreover, what happens when we update a complex observable element?
      //If listeners also listen on the element itself, they are gonna get too many notifications.
      //Otherwise, they might ignore the notification from us... it's not clear.
      case Update(old, curr) => publish(Update(f(old), f(curr)))

      // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
      // Implement these in a superclass for these maintenance operations.
      //case Update(old, curr) => publish(Script(Remove(f(old)), Include(f(curr))))
      /*case Update(old, curr) =>
        notify(pub, Remove(old))
        notify(pub, Include(curr))*/
      case Script(msgs @ _*) => msgs foreach (notify(pub, _))
    }
  }
}

//Trait implementing incremental view maintenance for WithFilter operations.
case class WithFilterMaintainer[T, Repr](p: T => Boolean) extends Subscriber[Message[T], Repr] with Publisher[Message[T]] {
  override def notify(pub: Repr, evt: Message[T]) {
    evt match {
      case Include(v) => if (p(v)) publish(Include(v))
      case Remove(v) => if (p(v)) publish(Remove(v))
      case Reset() => publish(Reset())

      // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
      // The handling here is valid more in general, but no batching is done.
      case Update(old, curr) =>
        notify(pub, Remove(old))
        notify(pub, Include(curr))
      case Script(msgs @ _*) => msgs foreach (notify(pub, _))
    }
  }
}

//Trait implementing incremental view maintenance for FlatMap operations.
case class FlatMapMaintainer[T, U, Repr](f: T => Traversable[U]) extends Subscriber[Message[T], Repr] with Publisher[Message[U]] {
  override def notify(pub: Repr, evt: Message[T]) {
    evt match {
      case Include(v) => publish(Script(f(v).toSeq map (Include(_)): _*))
      case Remove(v) => publish(Script(f(v).toSeq map (Remove(_)): _*))
      case Reset() => publish(Reset())
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
      case Script(msgs @ _*) => msgs foreach (notify(pub, _))
    }
  }
}

// TODO: add a trait which materializes update, and one which implements maintainance of unification.
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?
