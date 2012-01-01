package ivm.expressiontree

case class LEq[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends BinaryOpExp[T, T, Boolean](x, y) {
  def interpret() = ord.lteq(x.interpret(), y.interpret())
  override def copy(x: Exp[T], y: Exp[T]) = LEq(x, y)
}

case class Less[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends BinaryOpExp[T, T, Boolean](x, y) with BinaryOpTrait2[Exp[T], Exp[T], Boolean, Less[T]] {
  def interpret() = ord.lt(x.interpret(), y.interpret())
  override def copy(x: Exp[T], y: Exp[T]) = Less(x, y)
}
