package ivm
package expressiontree

import collection.mutable.HashMap

/**
 * User: pgiarrusso
 * Date: 27/8/2011
 */

private[expressiontree] object IncrementalResult {
  // Given e.g. coll2 = MapOp(coll@IncHashSet(_), FuncExp(...)), coll2 is the child and coll is the parent (here, the root).
  // Do not return duplicates!
  def findChildrenOfRoots(child: Exp[_], e: Exp[_]): Set[Exp[_]] = {
    (if (e.isRoot)
      Set(child)
    else
      Set.empty) ++
      (e.roots flatMap (findChildrenOfRoots(e, _)))
  }

  /*
   * Here we have two different tasks to take care of. Each child must start listening onto its parent; moreover,
   * for each root, we need to inform its children of addition of their elements
   * Using findRoots for both is a bug. However, for strange reasons, the code currently works by performing both
   * operations on all child-parent couples. Probably this is because of the iteration order: roots will be returned at the end by findRoots,
   * which means that most notify calls will send no notifications. Additionally, even when sending extra notifications,
   * the containing IncrementalResult will contain the right elements: only their presence count will be too high, so we
   * need to remove the initial elements to show the problem. Add a test for this.
   */
  def startListeners(initialRoot: Exp[_]) {
    initialRoot.visitPreorderRoots(_ activateIVM())
  }

  def propagateRootsElements(initialChild: Exp[_], initialRoot: Exp[_]) {
    for (child <- findChildrenOfRoots(initialChild, initialRoot))
      child pullAndPropagateContent()
  }
}
/**
 * A class representing an intermediate or final result of an incremental query.
 * XXX: This class mixes two roles - its constructor activates listeners for IVM, and its listener materialize the
 * collection. We might need the second function without necessarily needing the first, but since its constructor calls
 * base.interpret(), the constructor cannot be invoked on open terms - thus, it can't be used within the body of closures.
 * Moreover, the first function does not seem to conceptually require a new class. The only problem in separating the
 * two functions is that we'd need roots to _not_ be empty for the visit by startListeners, but the class should still
 * be considered a root for propagateRootsElements. We'd thus need to make them call different methods for the visit.
 */
// XXX: SetProxy is not entirely
// satisfactory - we want maybe something more like SetForwarder, which does not forward calls creating sequences of the
// same type. OTOH, this methods allows accessing the underlying data at all.
class IncrementalResult[T](val base: Exp[Traversable[T]]) extends NullaryExp[Traversable[T]] //The use of NullaryExp is fishy.
  with Queryable[T, collection.SetProxy[T]]
  with OneRootTraversableMaintainer[T, Traversable[T], Traversable[T]]
  with collection.SetProxy[T] //I mean immutable.SetProxy[T], but that requires an underlying immutable Set.
  // I'll probably end up with forwarding most basic methods manually, and implementing the others through SetLike.
  // Or we'll just support incremental query update for all methods.
{
  import IncrementalResult._
  var set = new HashMap[T, Int]
  base addSubscriber this
  //initializes listeners in the contained expression and makes initial elements flow through the query tree.
  startListeners(base)
  propagateRootsElements(this, base)

  //Our constructor calls startListeners(this, base); thus, base should not be visited again when creating another
  //IncrementalResult which depends on this object.
  //The same holds for propagateRootsElements, but for a more complex reason: notifications for elements already present
  //are not propagated, since they are considered duplicate.
  override def isRoot = true
  //From SetProxy
  override def self = set.keySet

  private[this] def count(v: T) = set.getOrElse(v, 0)
  private[this] def logAndPublish(evt: TravMessage[T]) {
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
            logAndPublish(evt)
          set(v) = vCount + 1
        case Remove(v) =>
          val vCount = count(v) - 1
          if (vCount > 0)
            set(v) = vCount
          else {
            logAndPublish(evt)
            set -= v
          }

        case Reset =>
          logAndPublish(evt)
          set.clear()
        // These two cases are quite common: they basically mean that no special handling is provided for bulk events.
        // The handling here is valid more in general, but no batching is done.
        case Update(old, curr) =>
          notify(pub, Seq(Remove(old), Include(curr)))
      }
    }
  }
  override def toString() = "IncrementalResult(" + self.toString + ")"
}
