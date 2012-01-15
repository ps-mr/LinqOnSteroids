package ivm.expressiontree

//Exploration into the design space.
case class Fix[T](col: Exp[Traversable[T]], f: FuncExp[Traversable[T], Traversable[T]])
  extends BinaryOp[Exp[Traversable[T]], FuncExp[Traversable[T], Traversable[T]], Traversable[T], Fix[T]](col, f)
{
  def copy(col: Exp[Traversable[T]], f: FuncExp[Traversable[T], Traversable[T]]) = Fix(col, f)
  override def interpret() = {
    // XXX: very very basic implementation of fixpoint computation
    var newV = col.interpret()
    var curr = newV
    do {
      curr = newV
      newV = f.interpret()(curr)
    } while (newV != curr)
    newV
  }
}
