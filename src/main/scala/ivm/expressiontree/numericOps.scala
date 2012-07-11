package ivm.expressiontree

import Numeric.Implicits._

//Root node for all binary, associative and commutative operations. The
//intuition is that many operations (including optimizations) might apply
//for all of those - e.g. expression normalization.
trait CommutativeOp[T, Self <: Exp[T]] extends Arity2OpSymmExp[T, T, Self] {
  this: Self =>
}

abstract class CommOp[T, Self <: Exp[T]](val t1: Exp[T], val t2: Exp[T]) extends CommutativeOp[T, Self] {
  this: Self =>
  def op: (T, T) => T
  def interpret() = op(t1.interpret(), t2.interpret())
}

//Note: the isNum member is referenced by Optimization, thus cannot be transformed into a context bound.
case class Plus[T](override val t1: Exp[T], override val t2: Exp[T])(implicit val isNum: Numeric[T]) extends CommOp[T, Plus[T]](t1, t2) with InfixPrinting {
  def op = _ + _
  def operator = "+"
  def copy(x: Exp[T], y: Exp[T]) = Plus(x, y)
}

case class Times[T](override val t1: Exp[T], override val t2: Exp[T])(implicit val isNum: Numeric[T]) extends CommOp[T, Times[T]](t1, t2) with InfixPrinting {
  def op = _ * _
  def operator = "*"
  def copy(x: Exp[T], y: Exp[T]) = Times(x, y)
}

case class Negate[T](override val t1: Exp[T])(implicit val isNum: Numeric[T]) extends Arity1OpExp[T, T, Negate[T]](t1) with PrefixPrinting {
  def copy(t1: Exp[T]) = Negate(t1)
  def prefix = "-"
  def interpret() = - t1.interpret()
}
