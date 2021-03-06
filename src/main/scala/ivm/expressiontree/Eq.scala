package ivm.expressiontree

case class Eq[/*@specialized(Int, Boolean, Double)*/ T](t1: Exp[T], t2: Exp[T]) extends Arity2OpSymmExp[T, Boolean, Eq[T]] with InfixPrinting {
  def copy(x: Exp[T], y: Exp[T]) = Eq(x, y)
  def operator = "=="
  def interpret() = t1.interpret() == t2.interpret()
  //The implementation below could work better when specialization is active.
  // But apparently that annotation is not enough. Retry by specializing also Exp[T] for T = Int, Boolean, ...
  //def interpret() = x.interpret() == y.interpret();
}
