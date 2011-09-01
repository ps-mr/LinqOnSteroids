package ivm.expressiontree

case class Eq[/*@specialized(Int, Boolean, Double)*/ T](x: Exp[T], y: Exp[T]) extends BinaryOpSymmExp[T, Boolean](x, y) {
  def copy(x: Exp[T], y: Exp[T]) = Eq(x, y)
  def interpret() = x.interpret().equals(y.interpret());
  //The implementation below could work better when specialization is active.
  // But apparently that annotation is not enough. Retry by specializing also Exp[T] for T = Int, Boolean, ...
  //def interpret() = x.interpret() == y.interpret();
}
