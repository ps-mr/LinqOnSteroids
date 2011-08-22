package ivm.expressiontree

case class Map[S, T](col: QueryReifier[S], f: FuncExp[S,T]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => Map(v(0).asInstanceOf[QueryReifier[S]], 
                                       v(1).asInstanceOf[FuncExp[S,T]])
  override def exec() = col.exec().map(f.interpret())
}
