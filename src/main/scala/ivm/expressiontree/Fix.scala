package ivm.expressiontree

//Exploration into the design space.
case class Fix[T](col: QueryReifier[T], f: FuncExp[Traversable[T],Traversable[T]]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => Fix(v(0).asInstanceOf[QueryReifier[T]],
                                       v(1).asInstanceOf[FuncExp[Traversable[T],Traversable[T]]])
  override def exec() = {
    // XXX: very very basic implementation of fixpoint computation
    var newV = col.exec()
    var curr = newV
    do {
      curr = newV
      newV = f.interpret()(curr)
    } while (newV != curr)
    newV
  }
}

case class FixWithReifiers[T](col: QueryReifier[T], f: FuncExp[QueryReifier[T],QueryReifier[T]]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => FixWithReifiers(v(0).asInstanceOf[QueryReifier[T]],
                                       v(1).asInstanceOf[FuncExp[QueryReifier[T],QueryReifier[T]]])
  override def exec() = {
    // XXX: another very very basic implementation. However, here we build QueryReifiers and compare their results. This might be potentially more efficient
    var newV = col
    var curr = newV
    do {
      curr = newV
      newV = f.interpret()(curr)
    } while (newV.interpret() != curr.interpret())
    newV.exec()
  }
}
