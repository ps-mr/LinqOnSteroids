package ivm.expressiontree

import collection.mutable.HashMap
import collection.generic.CanBuildFrom
import collection.TraversableLike

// All the maintainer classes/traits (MapMaintener, FilterMaintainer, FlatMapMaintainer) have a common structure of
// "message transformers". That can be probably abstracted away: they should have a method
// producedMessages: TravMessage[T] => Seq[TravMessage[T]], we should remove Script[T], implement publish in term of it in type:
// Forwarder[T, U, Repr] extends Subscriber[Seq[TravMessage[T]], Repr] with DefaultPublisher[Seq[TravMessage[U]]]
// notify(evts: Seq[TravMessage[T]]) = publish(evts flatMap producedMessages)
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
// XXX: We could use MonadPlus.mplus (or equivalent) for composing observables, e.g. in Union.

// Let us first implement incremental view maintenance for sets.

//Trait implementing incremental view maintenance for MapOp operations
trait MapOpMaintainer[T, U, Repr] extends EvtTransformer[T, U, Repr] {
  def fInt: T => U
  override def transformedMessages(pub: Repr, evt: TravMessage[T]) = {
    evt match {
      case Include(v) => Seq(Include(fInt(v)))
      case Remove(v) => Seq(Remove(fInt(v)))

      //Optionally, we can preserve update events as such.
      //case Update(old, curr) => publish(Update(fInt(old), fInt(curr)))
      //It's not clear whether this is any better. Moreover, what happens when we update a complex observable element?
      //If listeners also listen on the element itself, they are gonna get too many notifications.
      //Otherwise, they might ignore the notification from us... it's not clear.

      case _ => defTransformedMessages(pub, evt)
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
trait FilterMaintainer[T, Repr] extends EvtTransformer[T, T, Repr] {
  def pInt: T => Boolean
  override def transformedMessages(pub: Repr, evt: TravMessage[T]) = {
    evt match {
      case Include(v) => if (pInt(v)) Seq(evt) else Seq.empty
      case Remove(v) => if (pInt(v)) Seq(evt) else Seq.empty
      case _ => defTransformedMessages(pub, evt)
    }
  }
}

trait UnionMaintainer[T, Repr <: Exp[Traversable[T]]] extends EvtTransformer[T, T, Repr] {
  //XXX: the fact that transformedMessages does not get pub is broken.
  override def notify(pub: Repr, evts: Seq[TravMessage[T]]) = {
    val fixedEvts = for (evt <- evts)
      yield
      evt match {
        case Reset => pub.interpret().toSeq map (Remove(_))
        case _ => Seq(evt)
      }
    super.notify(pub, fixedEvts.flatten)
  }

  override def transformedMessages(pub: Repr, evt: TravMessage[T]) = {
    evt match {
      case Include(_) | Remove(_) => Seq(evt)
      case _ => defTransformedMessages(pub, evt)
    }
  }
}

//Trait implementing incremental view maintenance for FlatMap operations.
trait FlatMapMaintainer[T, U, Repr, That <: Traversable[U]] extends EvtTransformer[T, U, Repr] {
  self: Exp[Traversable[U]] => //? [T]? That's needed to subscribe.
  def fInt: T => Exp[TraversableOnce[U]]
  var subCollCache = new HashMap[T, Exp[TraversableOnce[U]]]
  val subCollListener: MsgSeqSubscriber[TraversableOnce[U], Exp[TraversableOnce[U]]] =
    new MsgSeqSubscriber[TraversableOnce[U], Exp[TraversableOnce[U]]] with Serializable {
      override def notify(pub: Exp[TraversableOnce[U]], evts: Seq[Message[TraversableOnce[U]]]) = {
        for (evt <- evts) {
          evt match {
            case e@Include(_) => publish(e)
            case e@Remove(_) => publish(e)
            case e@Update(_, _) => publish(e)
            //XXX very important! This can only work because we send the
            //modification before performing the update. Otherwise, we could pack the list of elements within
            //Reset notifications
            case Reset => publish(pub.interpret().toSeq map (Remove(_)))
          }
        }
      }
    }
  private def process(v: T) = {
    val fV = subCollCache.getOrElseUpdate(v, fInt(v))
    fV addSubscriber subCollListener
    IncrementalResult.startListeners(fV)
    fV.interpret().toSeq map (Include(_))
  }
  //To be invoked by the constructor with the initial elements.
  protected def initListening(values: Traversable[T]) {
    for (v <- values) {
      process(v)
    }
  }
  override def transformedMessages(pub: Repr, evt: TravMessage[T]) = {
    evt match {
      case Include(v) =>
        process(v)
      case Remove(v) =>
        //val fV = fInt(v) //fV will not always return the _same_ result. We
        //need a map from v to the returned collection - as done in LiveLinq
        //anyway.
        val fV = subCollCache(v)
        //cache -= v //This might actually be incorrect, if v is included twice in the collection. This is a problem!
        fV removeSubscriber subCollListener
        fV.interpret().toSeq map (Remove(_))
      case _ => defTransformedMessages(pub, evt)
      /*
      //Here we cannot implement an update by sending an update of the mapped element. But we should.
      //case Update(old, curr) => publish(Update(f(old), f(curr)))
      */
    }
  }
}

trait Maintainer[SrcMsg, Src <: SrcMsg, +Res] extends MsgSeqSubscriber[SrcMsg, Exp[Src]] with Exp[Res] {
  type RootType = Src
  private[ivm] override def activateIVM() { roots foreach (startListeningOn(_)) }
  def startListeningOn(root: Exp[Src]) {
    if (Debug.verbose) {
      val asString =
        try {
          this.toString
        } catch {
          case _ => ""
        }
      println("%s startListeningOn %s" format (asString, root))
    }
    root addSubscriber this
  }
}

trait TraversableMaintainer[SrcMsg, Src <: Traversable[SrcMsg], +Res] extends Maintainer[Traversable[SrcMsg], Src, Res] {
  private[ivm] override def pullAndPropagateContent() {
    for (root <- roots) {
      notify (root, root.interpret().toSeq.map(Include(_)))
    }
  }
}

trait OneRootTraversableMaintainer[SrcMsg, Src <: Traversable[SrcMsg], +Res] extends TraversableMaintainer[SrcMsg, Src, Res] {
  val base: Exp[Src]
  /*private[ivm]*/ override def roots = Seq(base)
}

//Don't make Repr so specific as IncCollectionReifier. Making Repr any specific
//is entirely optional - it just enables the listener to get a more specific
//type for the pub param to notify(), if he cares.
class MapOpMaintainerExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 U, That <: Traversable[U]](base: Exp[Repr], f: FuncExp[T, U])
                         (implicit override protected val c: CanBuildFrom[Repr, U, That]) extends MapOp[T, Repr, U, That](base, f)
    with MapOpMaintainer[T, U, Exp[Repr]] with OneRootTraversableMaintainer[T, Repr, That]
    with MsgSeqPublisher[That, MapOpMaintainerExp[T, Repr, U, That]] {
  override def fInt = f.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, U]) = new MapOpMaintainerExp[T, Repr, U, That](base, f)
}

class FlatMapMaintainerExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 U, That <: Traversable[U]](base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]])
                         (implicit override protected val c: CanBuildFrom[Repr, U, That]) extends FlatMap[T, Repr, U, That](base, f)
    with FlatMapMaintainer[T, U, Exp[Repr], That] with OneRootTraversableMaintainer[T, Repr, That]
    with MsgSeqPublisher[That, FlatMapMaintainerExp[T, Repr, U, That]] {
  override def fInt: T => Exp[TraversableOnce[U]] = {
    import Lifting._
    f(_)
  }

  override def copy(base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]]) = new FlatMapMaintainerExp[T, Repr, U, That](base, f)
}

class FilterMaintainerExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr], p: FuncExp[T, Boolean]) extends Filter[T, Repr](base, p)
    with FilterMaintainer[T, Exp[Repr]] with OneRootTraversableMaintainer[T, Repr, Repr] with MsgSeqPublisher[Repr, FilterMaintainerExp[T, Repr]] {
  override def pInt = p.interpret()
  override def copy(base: Exp[Repr], f: FuncExp[T, Boolean]) = new FilterMaintainerExp[T, Repr](base, f)
}

//XXX: where does this belong to? I was talking about materialization of updates (i.e. IncrementalResult -> Force) and
//"maintenance of unification" (???).
// Probably they can be both implemented together. Look into the other implementation, use bags or sth.
// There was a use-case I forget where other context information, other than a simple count, had to be stored.
// Was it a path in a hierarchical index?

class UnionMaintainerExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr], That <: Traversable[T]](base: Exp[Repr], that: Exp[Traversable[T]])
  (implicit c: CanBuildFrom[Repr, T, That]) extends
  Union[T, Repr, That](base, that)(c) with UnionMaintainer[T, Exp[Traversable[T]]] with TraversableMaintainer[T, Traversable[T], That]
  with MsgSeqPublisher[That, UnionMaintainerExp[T, Repr, That]]
{
  /*private[ivm]*/ override def roots = Seq(base, that)
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = new UnionMaintainerExp[T, Repr, That](base, that)
}

class NotMaintainerExp(b: Exp[Boolean]) extends Not(b) with EvtTransformerEl[Boolean, Boolean, Exp[Boolean]] with CachingExp[Boolean] {
  def notifyEv(pub: Exp[Boolean], evt: Message[Boolean]) {
    evt match {
      case NewVal(v) =>
        cache = Some(!v)
      case UpdateVal(_, v) =>
        cache = Some(!v)
    }
  }
}

// vim: set ts=4 sw=4 et:
