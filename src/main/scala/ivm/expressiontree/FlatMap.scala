package ivm.expressiontree

case class FlatMap[S, T](col: QueryReifier[S], f: FuncExp[S,QueryReifier[T]]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => FlatMap(v(0).asInstanceOf[QueryReifier[S]], v(1).asInstanceOf[FuncExp[S,QueryReifier[T]]])
  override def containsExp[X](e: Exp[X]) = f.isOrContains(e) || col.isOrContains(e)
  override def exec() = col.exec().flatMap(x => f.interpret()(x).exec())
}
