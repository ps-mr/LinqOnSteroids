package ivm.expressiontree

case class Eq[/*@specialized(Int, Boolean, Double)*/ T](t1: Exp[T], t2: Exp[T]) extends BinaryOpSymmExp[T, Boolean, Eq[T]] {
  def copy(x: Exp[T], y: Exp[T]) = Eq(x, y)
  def interpret() = {
    val v1 = t1.interpret()
    val v2 = t2.interpret()
    v1 == v2

  };
  //The implementation below could work better when specialization is active.
  // But apparently that annotation is not enough. Retry by specializing also Exp[T] for T = Int, Boolean, ...
  //def interpret() = x.interpret() == y.interpret();
}
