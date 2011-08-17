package ivm
package expressiontree

import indexing.HashIndex

case class IndexAt[T,S](index: HashIndex[T,S], y: Exp[S]) extends QueryOp[T]  {
  def children = Seq(y)
  def genericConstructor = (v) => IndexAt(index, v(0).asInstanceOf[Exp[S]])
  override def exec(isLazy: Boolean) = index(y.interpret())

}
