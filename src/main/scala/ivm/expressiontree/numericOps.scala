package ivm.expressiontree

import Numeric.Implicits._
import math.{Integral, Fractional}

//Root node for all binary, associative and commutative operations. The
//intuition is that many operations (including optimizations) might apply
//for all of those - e.g. expression normalization.
trait CommutativeOp[T, Self <: Exp[T]] extends Arity2OpSymmExp[T, T, Self] {
  this: Self =>
}

trait BinOp[T, Self <: Exp[T]] extends CommutativeOp[T, Self] {
  this: Self =>
  def op: (T, T) => T
  def interpret() = op(t1.interpret(), t2.interpret())
}

trait CommOp[T, Self <: Exp[T]] extends BinOp[T, Self] with CommutativeOp[T, Self] {
  this: Self =>
}

//Note: the isNum member is referenced by Optimization, thus cannot be transformed into a context bound.
case class Plus[T](t1: Exp[T], t2: Exp[T])(implicit val isNum: Numeric[T]) extends BinOp[T, Plus[T]] with InfixPrinting {
  def op = _ + _
  def operator = "+"
  def copy(x: Exp[T], y: Exp[T]) = Plus(x, y)
}

case class Times[T](t1: Exp[T], t2: Exp[T])(implicit val isNum: Numeric[T]) extends BinOp[T, Times[T]] with InfixPrinting {
  def op = _ * _
  def operator = "*"
  def copy(x: Exp[T], y: Exp[T]) = Times(x, y)
}

case class Negate[T](t1: Exp[T])(implicit val isNum: Numeric[T]) extends Arity1OpExpTrait[T, T, Negate[T]] with PrefixPrinting {
  def copy(t1: Exp[T]) = Negate(t1)
  def prefix = "-"
  def interpret() = - t1.interpret()
}

case class Div[T](t1: Exp[T], t2: Exp[T])(implicit val isFrac: Fractional[T]) extends BinOp[T, Div[T]] with InfixPrinting {
  import Fractional.Implicits._
  def op = _ / _
  def operator = "/"
  def copy(x: Exp[T], y: Exp[T]) = Div(x, y)
}

case class Mod[T](t1: Exp[T], t2: Exp[T])(implicit val isIntegral: Integral[T]) extends BinOp[T, Mod[T]] with InfixPrinting {
  import Integral.Implicits._
  def op = _ % _
  def operator = "%"
  def copy(x: Exp[T], y: Exp[T]) = Mod(x, y)
}
