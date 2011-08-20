package ivm.expressiontree

case class FlatMap[S, T](col: Exp[Traversable[S]], f: FuncExp[S,Traversable[T]]) extends Exp[Traversable[T]] {
  def children = Seq(col,f)
  def genericConstructor = (v) => FlatMap(v(0).asInstanceOf[Exp[Traversable[S]]],
                                          v(1).asInstanceOf[FuncExp[S,Traversable[T]]])
  override def interpret() = col.interpret().flatMap((x) => f.interpret()(x))
}
