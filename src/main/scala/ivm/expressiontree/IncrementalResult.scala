package ivm
package expressiontree

import collection.mutable.HashMap

/**
 * User: pgiarrusso
 * Date: 27/8/2011
 */

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

  private[ivm] def startListeners(e: Exp[_]) {
    def startSubListeners(e: Exp[_]) {
      for (c <- e.children) {
        startListeners(c)
      }
    }
    e match {
      case m: Maintainer[_] =>
        m.startListening()
        startSubListeners(e)
      case f: FuncExp[_, _] =>
        /*
        //Must transform f, such that listeners are started on its result! Hmm...
        //But again, that should not happen if we print the tree again. Doh!
        f.f andThen {
          e =>
            startListeners(e)
            e
        }*/
        f.interpretHook = Some(startListeners(_)) //Evil hack, I know.
        //Here we must not visit f children, i.e. its body!
      case _ =>
        startSubListeners(e)
    }
  }
  startListeners(inner)

  //From SetProxy
  override def self = set.keySet

  private[this] def count(v: T) = set.getOrElse(v, 0)
  private[this] def logPublish(evt: Message[T]) {
    if (Debug.verbose)
      println("publish(%s)" format evt)
    publish(evt)
  }

  override def notify(pub: QueryReifier[T], evts: Seq[Message[T]]) {
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
