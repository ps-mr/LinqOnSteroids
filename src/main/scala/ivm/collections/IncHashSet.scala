package ivm
package collections

import expressiontree.{IncQueryable, ObservableSet}

import collection.generic.{CanBuildFrom, MutableSetFactory, GenericSetTemplate}
import collection.mutable.{SetLike, HashSet}

class IncHashSet[T] extends HashSet[T] with ObservableSet[T]
   with IncQueryable[T, HashSet[T]]
   with /*extra templates:*/ GenericSetTemplate[T, IncHashSet] with SetLike[T, IncHashSet[T]] {
  type Pub <: IncHashSet[T] //Two different definitions of Pub are inherited, this one is a common subtype.
  override def companion = IncHashSet
}

// Next TODO: try to hide from the public interface of IncHashSet the methods from HashSet, so that people must call
// asCollection. Not sure whether it's a good idea. A possibly more sensible alternative is to hide methods from
// IncQueryReifier, so that people have to call asQueryable to get them.

object IncHashSet extends MutableSetFactory[IncHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IncHashSet[A]] = setCanBuildFrom[A]
  override def empty[A] = new IncHashSet[A]
  // XXX: without the above definition the code compiles, but empty is defined through newBuilder, and newBuilder
  // through empty, because one of the two is supposed to be overriden; not doing so results in infinite recursion.
  // TODO: write test for that.
}

