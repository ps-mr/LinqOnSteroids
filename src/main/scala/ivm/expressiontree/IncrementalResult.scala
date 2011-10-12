package ivm
package expressiontree

import collection.mutable.HashMap

/**
 * User: pgiarrusso
 * Date: 27/8/2011
 */

object IncrementalResult {
  private[this] def startListener(e: Exp[_]) {
    e match {
      case m: Maintainer[_, _] =>
        m.startListening() //i.e. m.base subscribe this; add notify(m.base, m.base.interpret().toSeq.map(Include(_))) or the like on the bottom.

      case f@FuncExp(_: (Exp[_] => Maintainer[_, _])) => //XXX broken
        f setInterpretHook Some((x: Exp[_]) => startListeners(None, x.asInstanceOf[Maintainer[Traversable[Any], Traversable[Any]]])) //Evil hack, I know. Have FlatMapMaintainer (but actually, all Maintainers) do that.
      case _ =>
    }
  }

  def findRoots[T, U](parent: Option[Exp[Traversable[_]]], e: Maintainer[Traversable[T], Traversable[U]]/*Exp[Traversable[_]] with MsgSeqSubscriber[_, _]*/): Seq[(Option[Exp[Traversable[_]]], Exp[Traversable[_]])] = {
    if (e.roots.isEmpty)
      Seq((parent, e))
    else {
      val newParent =
        if (e.isInstanceOf[MsgSeqSubscriber[_, _]])
          Some(e.asInstanceOf[Exp[Traversable[_]]])
        else None //parent //returning parent causes run-time type errors (ClassCastExceptions).
      Seq(e.base) flatMap ((x: Exp[Traversable[T]]) => findRoots(newParent, x.asInstanceOf[Maintainer[Traversable[Any], Traversable[Any]]]))
    }

  }

  def newStartListeners[T, U](parent: Option[Exp[Traversable[U]]], e: Maintainer[Traversable[T], Traversable[U]]) {
    //XXX: what if a collection appears multiple times in the tree? Solution: we get it with multiple parents.
    val roots = findRoots(parent, e) //Instead, fix startListener.
    for ((Some(p), root: Exp[Traversable[t]]) <- roots) {
      p match {
        case parent: MsgSeqSubscriber[Traversable[`t`], Exp[Traversable[`t`]]] =>
          root subscribe parent
          parent notify (root, root.interpret().toSeq.map(Include(_))) //This line is correct, but implies that parent is
          // a direct child of root, so that parent accepts notifications from child.
          // This can cause run-time type errors with a tricked findRoots.
          // We need to use EvtTransformer here to ensure type-safety.
      }
    }
  }

  def oldStartListeners(e: Exp[_]) {
    e visitPreorderClosedChildren startListener
  }

  def startListeners[T, U](parent: Option[Exp[Traversable[U]]], e: Maintainer[Traversable[T], Traversable[U]]) {
    newStartListeners(parent, e)
    oldStartListeners(e)
  }
}
/**
 * A class representing an intermediate or final result of an incremental query.
 */
// XXX: SetProxy is not entirely
// satisfactory - we want maybe something more like SetForwarder, which does not forward calls creating sequences of the
// same type. OTOH, this methods allows accessing the underlying data at all.
class IncrementalResult[T, U](val inner: Maintainer[Traversable[U], Traversable[T]]) extends NullaryExp[Traversable[T]]
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
  //XXX: I now believe this is a hack, in essence. I should not rely on interpret();
  // I should rather trigger updates starting from the root collection.
  // See FlatMapMaintainer.initListening() for the hack currently compensating this problem.

  // It is crucial to have this statement only here after construction
  notify(inner, inner.interpret().toSeq.map(Include(_)))

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
