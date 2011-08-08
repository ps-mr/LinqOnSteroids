package ivm
package collections

import expressiontree.QueryReifier

//Reifier on collections (in particular, Traversable). Reifiers on other data sources are also possible.

class CollectionReifier[T](val innercol: scala.collection.Traversable[T]) extends QueryReifier[T] {
  override def exec(isLazy: Boolean) = if (isLazy) innercol.view else innercol

  def interpret() = this 
  def children = Seq()
  def genericConstructor = (v) => this
}
