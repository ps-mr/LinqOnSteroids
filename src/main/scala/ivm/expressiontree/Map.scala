package ivm.expressiontree

import Lifting._

case class Map[S, T](col: Exp[Traversable[S]], f: FuncExp[S,T]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => Map(v(0).asInstanceOf[QueryReifier[S]], 
                                       v(1).asInstanceOf[FuncExp[S,T]])
  override def exec(isLazy: Boolean) = col.exec(isLazy).map(f.interpret())
}
