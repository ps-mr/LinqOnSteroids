package ivm.expressiontree

case class MapOp[S, T](col: Exp[Traversable[S]], f: FuncExp[S,T]) extends Exp[Traversable[T]] {
  def children = Seq(col,f)
  def genericConstructor = (v) => MapOp(v(0).asInstanceOf[Exp[Traversable[S]]],
                                       v(1).asInstanceOf[FuncExp[S,T]])
  override def interpret() = col.interpret().map(f.interpret())
}
