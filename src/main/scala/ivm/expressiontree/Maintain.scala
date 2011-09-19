package ivm.expressiontree

import collection.mutable.HashMap

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
// This way, a pipeline of message transformers becomes simply a sequencing through >>= of monadic actions.
// However, since some reifiers will not work this way, we cannot enforce this structure.
// TODO: We could use MonadPlus.mplus for composing observables.

// Let us first implement incremental view maintenance for sets.

//Trait implementing incremental view maintenance for MapOp operations
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
            case Reset() => publish(pub.interpret().toSeq map (Remove(_)))
          }
        }
      }
    }
  //To be invoked by the constructor with the initial elements.
  protected def initListening(values: Traversable[T]) {
    for (v <- values) {
      //XXX copied from transformedMessages. Could I really reuse that as-is? I believe I cannot!
      //Basically, it doesn't make sense to generate the new events and then to drop them!
      //Plus, here I still need to call startListening.
      val fV = cache.getOrElseUpdate(v, fInt(v))
      fV subscribe subCollListener
      fV match {
        case m: Maintainer[_] => m.startListening()
      }
    }
  }
  override def transformedMessages(evt: Message[T]) = {
    evt match {
      case Include(v) =>
        val fV = cache.getOrElseUpdate(v, fInt(v))
        fV subscribe subCollListener
        fV.interpret().toSeq map (Include(_))
      case Remove(v) =>
        //val fV = fInt(v) //fV will not always return the _same_ result. We
        //need a map from v to the returned collection - as done in LiveLinq
        //anyway.
        val fV = cache(v)
        //cache -= v //This might actually be incorrect, if v is included twice in the collection. This is a problem!
        fV removeSubscription subCollListener
        fV.interpret().toSeq map (Remove(_))
      case _ => defTransformedMessages(evt)
      /*
      //Here we cannot implement an update by sending an update of the mapped element. But we should.
      //case Update(old, curr) => publish(Update(f(old), f(curr)))
      */
    }
  }
}

/*trait Maintainer[T, Repr <: QueryReifier[T]] extends MsgSeqSubscriber[T, Repr] {
  this: QueryReifier[T]#Sub =>
  val col: QueryReifier[T]

  //def uglyCast(v: Subscriber[Seq[Message[T]], QueryReifier[T]#Pub]): Subscriber[Seq[Message[T]], col.Pub] =
  //Equivalent to:
  //def uglyCast(v: Subscriber[Seq[Message[T]], QueryReifier[T]]): Subscriber[Seq[Message[T]], col.Pub] =
  //Which is wrong. Instead we need:
  def uglyCast(v: Subscriber[Seq[Message[T]], Repr]): Subscriber[Seq[Message[T]], col.Pub] =
    v.asInstanceOf[Subscriber[Seq[Message[T]], col.Pub]]
  /*
   * Why is that cast needed? Repr <: QueryReifier[T], col.Pub <: QueryReifier[T], but col.Pub and Repr are incomparable.
   * The cast would be unneeded if Subscriber[..., col.Pub] >: Subscriber[..., Repr], i.e. col.Pub <: Repr.
   * Therefore, let's set Repr = QueryReifier[T]
   */

  def startListening() {
    //col subscribe this.asInstanceOf[col.Sub]
    col subscribe uglyCast(this)
  }
}*/
trait Maintainer[T] {
  this: MsgSeqSubscriber[T, QueryReifier[T]] =>
  val col: QueryReifier[T]

  def startListening() {
    if (Debug.verbose)
      //println("Maintainer(col = %s) startListening" format col)
      println("%s startListening" format this)
    col subscribe this
  }
}

/*
//Don't make Repr so specific as IncCollectionReifier. Making Repr any specific
//is entirely optional - it just enables the listener to get a more specific
//type for the pub param to notify(), if he cares.
class MapOpMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,U]) extends MapOp[T,U](col, f)
    with MapMaintainer[T, U, QueryReifier[T]] with QueryReifier[U] with Maintainer[T] {
  override def fInt = f.interpret()
  //XXX: only the name of the constructed class changes
  override def genericConstructor =
      v => new MapOpMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, U]])
}
class FlatMapMaintainerExp[T,U](col: QueryReifier[T], f: FuncExp[T,QueryReifier[U]]) extends FlatMap[T,U](col, f)
    with FlatMapMaintainer[T, U, QueryReifier[T]] with QueryReifier[U] with Maintainer[T] {
  override def fInt = x => f.interpret()(x)
  //XXX ditto
  override def genericConstructor =
      v => new FlatMapMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, QueryReifier[U]]])

  //XXX this ensures that we listen on the results corresponding to the elements already present in col.
  //However, it is a hack - see IncrementalResult for discussion.
  initListening(col.interpret())
}
class WithFilterMaintainerExp[T](col: QueryReifier[T], p: FuncExp[T,Boolean]) extends WithFilter[T](col, p)
    with WithFilterMaintainer[T, QueryReifier[T]] with QueryReifier[T] with Maintainer[T] {
  override def pInt = p.interpret()
  //XXX ditto
  override def genericConstructor =
      v => new WithFilterMaintainerExp(v(0).asInstanceOf[QueryReifier[T]],
          v(1).asInstanceOf[FuncExp[T, Boolean]])
}
*/
// TODO: add a trait which implements maintenance of union.
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?

// vim: set ts=4 sw=4 et:
