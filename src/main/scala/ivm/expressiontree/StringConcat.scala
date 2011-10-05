package ivm.expressiontree

case class StringConcat(x: Exp[String], y: Exp[String]) extends BinaryOpExp[String, String, String](x, y) {
  def interpret() = x.interpret() + y.interpret()
  def copy(x: Exp[String], y: Exp[String]) = StringConcat(x, y)
}
