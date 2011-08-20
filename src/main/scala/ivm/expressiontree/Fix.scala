package ivm.expressiontree

//Exploration into the design space.

/* KO: commented out until needed and tested
case class Fix[T](col: ListQuery[T], f: FuncExp[Traversable[T],Traversable[T]]) extends Exp[Traversable[T]] {
  def children = Seq(col,f)
  def genericConstructor = (v) => Fix(v(0).asInstanceOf[ListQuery[T]],
                                       v(1).asInstanceOf[FuncExp[Traversable[T],Traversable[T]]])
  override def exec(isLazy: Boolean) = {
    // XXX: very very basic implementation of fixpoint computation
    var newV = col.exec(isLazy)
    var curr = newV
    do {
      curr = newV
      newV = f.interpret()(curr)
    } while (newV != curr)
    newV
  }
}

case class FixWithReifiers[T](col: ListQuery[T], f: FuncExp[ListQuery[T],ListQuery[T]]) extends QueryOp[T] {
  def children = Seq(col,f)
  def genericConstructor = (v) => FixWithReifiers(v(0).asInstanceOf[ListQuery[T]],
                                       v(1).asInstanceOf[FuncExp[ListQuery[T],ListQuery[T]]])
  override def exec(isLazy: Boolean) = {
    // XXX: another very very basic implementation. However, here we build QueryReifiers and compare their results. This might be potentially more efficient
    var newV = col
    var curr = newV
    do {
      curr = newV
      newV = f.interpret()(curr)
    } while (newV.interpret() != curr.interpret())
    newV.exec(isLazy)
  }
}
  */