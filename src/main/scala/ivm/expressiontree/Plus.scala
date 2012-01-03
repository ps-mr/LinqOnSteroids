package ivm.expressiontree

import Numeric.Implicits._

//Root node for all binary, associative and commutative operations. The
//intuition is that many operations (including optimizations) might apply
//for all of those - e.g. expression normalization.
trait CommutativeOp[T] extends BinaryOpExpTrait[T, T, T]

abstract class CommOp[T](x: Exp[T], y: Exp[T])(op: (T, T) => T) extends BinaryOpSymmExp[T, T](x, y) {
  def interpret() = op(x.interpret(), y.interpret())
}

//Note: the isNum member is referenced by Optimization, thus cannot be transformed into a context bound.
case class Plus[T](override val t1: Exp[T], override val t2: Exp[T])(implicit val isNum: Numeric[T]) extends CommOp[T](t1, t2)(_ + _) with CommutativeOp[T] {
  def copy(x: Exp[T], y: Exp[T]) = Plus(x, y)
}

case class Times[T](override val t1: Exp[T], override val t2: Exp[T])(implicit val isNum: Numeric[T]) extends CommOp[T](t1, t2)(_ * _) with CommutativeOp[T] {
  def copy(x: Exp[T], y: Exp[T]) = Times(x, y)
}

case class Negate[T: Numeric](override val t1: Exp[T]) extends UnaryOpExp[T, T](t1) {
  def copy(t1: Exp[T]) = Negate(t1)
  def interpret() = - t1.interpret()
}
