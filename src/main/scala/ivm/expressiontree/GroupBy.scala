package ivm.expressiontree


case class GroupBy[S, T](col: QueryReifier[S], f: FuncExp[S,T]) extends MapReifier[T,QueryReifier[S]] {
  def children = Seq(col,f)
  def genericConstructor = (v) => GroupBy(v(0).asInstanceOf[QueryReifier[S]], v(1).asInstanceOf[FuncExp[S,T]])

}