package ivm.expressiontree


case class And(x: Exp[Boolean], y: Exp[Boolean]) extends BinaryOpSymmExp[Boolean, Boolean](x, y) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = And(x, y)
  def interpret() = x.interpret() && y.interpret()
}

case class Or(x: Exp[Boolean], y: Exp[Boolean]) extends BinaryOpSymmExp[Boolean, Boolean](x, y) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = Or(x, y)
  def interpret() = x.interpret() || y.interpret()
}

case class Not(x: Exp[Boolean]) extends UnaryOpExp[Boolean, Boolean](x) {
  def copy(x: Exp[Boolean]) = Not(x)
  def interpret() = !x.interpret()
}