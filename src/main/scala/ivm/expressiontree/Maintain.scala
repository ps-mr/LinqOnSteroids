package ivm.expressiontree

import collection.mutable.{HashMap, Subscriber, Buffer, Set}
// Let us first implement incremental view maintenance for sets.

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
trait FlatMapMaintainer[T, U, Repr] extends EvtTransformer[T, U, Repr] {
  self: QueryReifier[U] => //? [T]? That's needed for the subscribe.
  def fInt: T => QueryReifier[U]
  var cache = new HashMap[T, QueryReifier[U]]
  val subCollListener: MsgSeqSubscriber[U, QueryReifier[U]] =
    new MsgSeqSubscriber[U, QueryReifier[U]] {
      override def notify(pub: QueryReifier[U], evts: Seq[Message[U]]) = {
        //publish(evts flatMap (evt => evt match {
        //evts foreach (evt => evt match {
        for (evt <- evts) {
          evt match {
            case Include(_) => publish(evt)
            case Remove(_) => publish(evt)
            case Update(_, _) => publish(evt)
            //XXX very important! This can only work because we send the
            //modification before performing the update
            case Reset() => publish(pub.exec().toSeq map (Remove(_)))
          }
        }
      }
    }
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) =>
        val fV = fInt(v)
        fV subscribe subCollListener
        fV.exec().toSeq map (Include(_))
      case Remove(v) =>
        //val fV = fInt(v) //fV will not always return the _same_ result. We
        //need a map from v to the returned collection - as done in LiveLinq
        //anyway.
        val fV = cache(v)
        fV removeSubscription subCollListener
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
  //XXX: only the name of the constructed class changes
  override def genericConstructor =
      v => new MapMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, U]])
}
class FlatMapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,QueryReifier[U]]) extends FlatMap[T,U](col, f)
with FlatMapMaintainer[T, U, QueryReifier[T]] with QueryReifier[U] {
  override def fInt = x => f.interpret()(x)
  //XXX ditto
  override def genericConstructor =
      v => new FlatMapMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, QueryReifier[U]]])
}
class WithFilterMaintainerExp[T](col: QueryReifier[T], p: FuncExp[T,Boolean]) extends WithFilter[T](col, p)
with WithFilterMaintainer[T, QueryReifier[T]] with QueryReifier[T] {
  override def pInt = p.interpret()
  //XXX ditto
  override def genericConstructor =
      v => new WithFilterMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, Boolean]])
}

// TODO: add a trait which implements maintenance of union.
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?

// vim: set ts=4 sw=4 et:
