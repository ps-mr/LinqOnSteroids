package ivm.expressiontree

import Lifting._

case class FlatMap[S, T](col: Exp[Traversable[S]], f: FuncExp[S,Traversable[T]]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => FlatMap(v(0).asInstanceOf[Exp[Traversable[S]]], v(1).asInstanceOf[FuncExp[S,Traversable[T]]])
  override def containsExp[X](e: Exp[X]) = f.isOrContains(e) || col.isOrContains(e)
  override def exec(isLazy: Boolean) = col.exec(isLazy).flatMap((x) => f.interpret()(x))


}
