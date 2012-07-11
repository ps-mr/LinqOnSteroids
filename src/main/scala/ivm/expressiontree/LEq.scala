package ivm.expressiontree

case class LEq[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends Arity2OpExp[T, T, Boolean, LEq[T]](x, y) with InfixPrinting {
  def interpret() = ord.lteq(x.interpret(), y.interpret())
  override def copy(x: Exp[T], y: Exp[T]) = LEq(x, y)
  def operator = "<="
}

case class Less[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends Arity2OpExp[T, T, Boolean, Less[T]](x, y) with InfixPrinting {
  def interpret() = ord.lt(x.interpret(), y.interpret())
  override def copy(x: Exp[T], y: Exp[T]) = Less(x, y)
  def operator = "<"
}
