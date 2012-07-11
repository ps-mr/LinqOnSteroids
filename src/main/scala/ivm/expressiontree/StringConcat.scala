package ivm.expressiontree

case class StringConcat(x: Exp[String], y: Exp[String]) extends Arity2OpExp[String, String, String, StringConcat](x, y) with InfixPrinting {
  def interpret() = x.interpret() + y.interpret()
  def copy(x: Exp[String], y: Exp[String]) = StringConcat(x, y)
  def operator = "+"
}
