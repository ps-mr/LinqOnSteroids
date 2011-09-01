package ivm.expressiontree

case class Plus[T](x: Exp[T], y: Exp[T])(implicit val isNum: Numeric[T]) extends BinaryOpSymmExp[T, T](x, y) {
  def interpret() = isNum.plus(x.interpret(), y.interpret())
  def copy(x: Exp[T], y: Exp[T]) = Plus(x, y)
}
