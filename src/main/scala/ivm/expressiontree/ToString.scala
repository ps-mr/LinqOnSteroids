package ivm.expressiontree

case class ToString[T](e: Exp[T]) extends Arity1OpExp[T, String, ToString[T]](e) with InfixPrinting {
  def interpret() = e.interpret().toString
  def copy(e: Exp[T]) = ToString(e)
  def operator = "toString"
}
