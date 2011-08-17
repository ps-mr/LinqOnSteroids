package ivm
package collections

import expressiontree.ChildlessQueryReifier

//Reifier on collections (in particular, Traversable). Reifiers on other data sources are also possible.

class CollectionReifier[T](val innercol: Traversable[T]) extends ChildlessQueryReifier[T] {
  override def exec(isLazy: Boolean) = if (isLazy) innercol.view else innercol
}
