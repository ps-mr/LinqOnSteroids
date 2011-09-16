package ivm.expressiontree


case class GroupBy[S, T](col: QueryReifier[S], f: FuncExp[S,T]) extends MapReifier[T,S] {
  override def children = Seq(col,f)
  override def genericConstructor = (v) => GroupBy(v(0).asInstanceOf[QueryReifier[S]], v(1).asInstanceOf[FuncExp[S,T]])
  override def exec = col.exec().groupBy(f.interpret())
}