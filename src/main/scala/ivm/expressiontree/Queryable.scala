package ivm
package expressiontree

import collections.CollectionReifier

/**
 * Trait to mix in with Scala collections to make them queryable.
 * An important point is that the original collection becomes queryable -
 * that is, that object identity is preserved.
 *
 * Key idea: make the same collection instance queryable.
 * Only a prototype.
 * Missing features:
 * - Lazy execution should return a more specific interface.
 *
 * - The built collection should possibly offer stronger interfaces.
 *   Relevant ones:
 *   - Traversable
 *   - Iterable (multiple iterators at the same time, looks easy)
 *   - Seq (sequential order is respected).
 *   - LinearSeq (linear access Seq)
 *   - IndexedSeq (random access Seq)
 * - When we execute the query, we should probably also register a notifier to update the generated collection.
 *
 */

//Probably Repr is also needed, especially if the produced Iterable must also offer stronger
//interfaces.
//TODO: add notification interface!
trait Queryable[T, Repr] extends ChildlessQueryReifier[T] /*with Publisher[Message[T]]*/ {
  self: Traversable[T] with Repr =>
  //type Pub = Queryable[T, Repr] //XXX? Should this be defined here already? Or should Pub be even more specific?
  def asQueryable: QueryReifier[T] = this
  def asCollection: Repr = this
  override def exec(isLazy: Boolean) = if (isLazy) this.view else this
  //This allows selecting early how the query is to be executed.
  // The alternative is to choose between exec(isLazy = true) and exec(isLazy = false).
  //def asQueryableLazy: QueryReifier[T] = new CollectionReifier[T](this.view)
  // XXX: this returns another object! So don't have it with this name, call it view - see below.
  //def view: TraversableView[T] = new CollectionReifier[T](self.view)
  //Can't have that type. Make it an overload - untested.

  // commented out by KO - need to check back with PG
  //def view2: QueryReifier[T] = new CollectionReifier[T](self.view)
}

trait IncQueryable[T, Repr] extends Queryable[T, Repr] with IncQueryReifier[T] {
  self: Traversable[T] with Repr =>
  override def asQueryable: IncQueryReifier[T] = this
}

// vim: set ts=4 sw=4 et:
