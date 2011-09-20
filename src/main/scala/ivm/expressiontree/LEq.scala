package ivm.expressiontree

case class LEq[T](x: Exp[T], y: Exp[T])(implicit val ord: Ordering[T]) extends BinaryOpExp[T, T, Boolean](x, y)(classManifest[Boolean]) {
  def interpret() = ord.lteq(x.interpret(), y.interpret())
  override def copy(x: Exp[T], y: Exp[T]) = LEq(x, y)
}
