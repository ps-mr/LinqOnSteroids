package ivm
package expressiontree

import collection.mutable.HashMap

/**
 * User: pgiarrusso
 * Date: 27/8/2011
 */

object IncrementalResult {
  // Given e.g. coll2 = MapOp(coll@IncHashSet(_), FuncExp(...)), coll2 is the child and coll is the parent (here, the root).
  def findRoots(child: Option[Exp[Traversable[_]]], e: Exp[Traversable[_]]): Seq[(Option[Exp[Traversable[_]]], Exp[Traversable[_]])] = {
    if (e.roots.isEmpty)
      Seq((child, e))
    else {
      val newParent =
        if (e.isInstanceOf[MsgSeqSubscriber[_, _]])
          Some(e.asInstanceOf[Exp[Traversable[_]]])
        else None //child //returning child causes run-time type errors (ClassCastExceptions).
      e.roots flatMap ((x: Exp[_]) => findRoots(newParent, x.asInstanceOf[Exp[Traversable[_]]]))
    }

  }

  def startListeners(child: Option[Exp[Traversable[_]]], e: Exp[Traversable[_]]) {
    //XXX: what if a collection appears multiple times in the tree? Solution: we get it with multiple children.
    val roots = findRoots(child, e) //Instead, fix startListener.
    for ((Some(p), root: Exp[Traversable[t]]) <- roots) {
      p match {
        case child: MsgSeqSubscriber[Traversable[`t`], Exp[Traversable[`t`]]] => //XXX: broken, but no counterexamples yet.
          root subscribe child
          child notify (root, root.interpret().toSeq.map(Include(_))) //This line is correct, but implies that child is
          // a direct child of root, so that child accepts notifications from it.
          // This constraint is not reflected in the type, thus we can write a version of findRoots
          // causing run-time type errors.
          // XXX: we probably need to use EvtTransformer here to express type-safety.
      }
    }
  }
}
/**
 * A class representing an intermediate or final result of an incremental query.
 */
// XXX: SetProxy is not entirely
// satisfactory - we want maybe something more like SetForwarder, which does not forward calls creating sequences of the
// same type. OTOH, this methods allows accessing the underlying data at all.
class IncrementalResult[T](val inner: Exp[Traversable[T]]) extends NullaryExp[Traversable[T]]
  with TravMsgSeqSubscriber[T, Exp[Traversable[T]]]
  with Queryable[T, collection.SetProxy[T]]
  with collection.SetProxy[T] //I mean immutable.SetProxy[T], but that requires an underlying immutable Set.
  // I'll probably end up with forwarding most basic methods manually, and implementing the others through SetLike.
  // Or we'll just support incremental query update for all methods.
{
  import IncrementalResult._
  var set = new HashMap[T, Int]
  inner subscribe this
  startListeners(Some(this), inner)

  //From SetProxy
  override def self = set.keySet

  private[this] def count(v: T) = set.getOrElse(v, 0)
  private[this] def logPublish(evt: TravMessage[T]) {
    if (Debug.verbose)
      println("publish(%s)" format evt)
    publish(evt)
  }

  override def notify(pub: Exp[Traversable[T]], evts: Seq[TravMessage[T]]) {
    if (Debug.verbose)
      println("%s notify(\n  pub = %s,\n  evts = %s\n)" format (this, pub, evts))
    for (evt <- evts) {
      evt match {
        case Include(v) =>
          val vCount = count(v)
          if (vCount == 0)
            logPublish(evt)
          set(v) = vCount + 1
        case Remove(v) =>
          val vCount = count(v) - 1
          if (vCount > 0)
            set(v) = vCount
          else {
            logPublish(evt)
            set -= v
          }

        case Reset() =>
          logPublish(evt)
          set.clear()
        // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
        // The handling here is valid more in general, but no batching is done.
        case Update(old, curr) =>
          notify(pub, Seq(Remove(old), Include(curr)))
        //case Script(msgs @ _*) => msgs foreach (notify(pub, _))
      }
    }
  }
  override def toString() = "IncrementalResult(" + self.toString + ")"
}
