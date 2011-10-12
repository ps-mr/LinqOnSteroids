package ivm
package expressiontree

import collection.mutable.{Subscriber, HashMap}

/**
 * User: pgiarrusso
 * Date: 27/8/2011
 */

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
  var set = new HashMap[T, Int]
  inner subscribe this
  startListeners(inner)
  //XXX: I now believe this is a hack, in essence. I should not rely on interpret();
  // I should rather trigger updates starting from the root collection.
  // See FlatMapMaintainer.initListening() for the hack currently compensating this problem.

  // It is crucial to have this statement only here after construction
  notify(inner, inner.interpret().toSeq.map(Include(_)))

  private[this] def startListener(e: Exp[_]) {
    e match {
      case m: Maintainer[_, _] =>
        m.startListening() //i.e. m.base subscribe this; add notify(m.base, m.base.interpret().toSeq.map(Include(_))) or the like on the bottom.

      case f: FuncExp[_, _] =>
        f.interpretHook = Some(startListeners(_)) //Evil hack, I know. Have FlatMapMaintainer (but actually, all Maintainers) do that.
      case _ =>
    }
  }

  def findRoots(parent: Option[Exp[_]], e: Exp[_]): (Seq[(Option[Exp[_]], Exp[_])]) = {
    if (e.roots.isEmpty)
      Seq((parent, e))
    else
      e.roots flatMap (findRoots(Some(e), _))
  }
  def findRoots2(parent: Option[Exp[Traversable[_]]], e: Exp[Traversable[_]]): Seq[(Option[Exp[Traversable[_]]], Exp[Traversable[_]])] = {
    if (e.roots.isEmpty)
      Seq((parent, e))
    else
      e.roots flatMap ((x: Exp[_]) => findRoots2(Some(e.asInstanceOf[Exp[Traversable[_]]]), x.asInstanceOf[Exp[Traversable[_]]]))
  }

  def startListeners(e: Exp[_]) {
    //XXX: what if a collection appears multiple times in the tree?
    val roots = findRoots2(None, inner) //Instead, fix startListener.
    for ((Some(p), /*r*/ root: Exp[Traversable[t]]) <- roots) {
      /*r match {
        case root/*: EvtTransformer[t, _/*u*/, repr] with  Exp[Traversable[u]] */ =>*/
      //Util.assertType[Exp[Traversable[_]]](root)
      p match {
        case parent: /*r*/ /*root.Sub with */Subscriber[Seq[TravMessage[`t`]], `root`.Pub/*Exp[Traversable[_]]*/] => //TravMsgSeqSubscriber[t, repr] =>
          /*r*/ root subscribe parent
          parent notify (root.asInstanceOf[/*r*/ root.Pub], root.interpret().toSeq.map(Include(_)))
      }
      //}
      /*
      //val root = r.asInstanceOf[MsgSeqSubscriber[T, Exp[T]] forSome { type T }]
      val root = r.asInstanceOf[parent.Sub] //XXX XXX XXX!
      val parent2 = parent.asInstanceOf[root.Sub] //XXX !
      root subscribe parent
      //parent subscribe root
      parent2.notify(root, root.asInstanceOf[Exp[Traversable[_]]].interpret().toSeq.map(Include(_)))
      //root.startListening()
      //root.sendContentToParents()
      */
    }

    e visitPreorderClosedChildren startListener
  }

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
