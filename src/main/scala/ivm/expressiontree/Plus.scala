package ivm.expressiontree

//Root node for all binary, associative and commutative operations. The
//intuition is that many operations (including optimizations) might apply
//for all of those - e.g. expression normalization.
trait CommutativeOp[T] extends Exp[T] {
  def x: Exp[T]
  def y: Exp[T]
}

case class Plus[T](x: Exp[T], y: Exp[T])(implicit val isNum: Numeric[T]) extends BinaryOpSymmExp[T, T](x, y) with CommutativeOp[T] {
  def interpret() = isNum.plus(x.interpret(), y.interpret())
  def copy(x: Exp[T], y: Exp[T]) = Plus(x, y)
}
