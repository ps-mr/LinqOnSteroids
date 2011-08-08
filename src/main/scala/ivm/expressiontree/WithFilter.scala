package ivm.expressiontree

case class WithFilter[S](col: QueryReifier[S], f: FuncExp[S,Boolean]) extends QueryOp[S] {
  def children = Seq(col,f)
  def genericConstructor = (v) => WithFilter(v(0).asInstanceOf[QueryReifier[S]], 
                                       v(1).asInstanceOf[FuncExp[S,Boolean]])
  //PG:
  //We cannot use withFilter here because it has a too weak type, but we can
  //call view.filter. This is still annoying though.
  //I also assume that view on a view returns the same view
  //I leave in this call because runtime goes from ~5s to ~4s.

  override def exec(isLazy: Boolean) = col.exec(isLazy).view.filter(f.interpret())
  //Now the lazification is introduced by an explicit View node created by QueryReifier.withFilter.
  //Extra lazification-reification pairs can then be deleted (if wanted) by the optimizer.
  //override def exec(isLazy: Boolean) = col.exec(isLazy).filter(f.interpret())
}
