package ivm.expressiontree

/**
 * Here I abstract building blocks for operation nodes, with associated boilerplate. They are especially useful because
 * they encapsulate type-unsafe boilerplate, i.e. the definitions of genericConstructor.
 * User: pgiarrusso
 * Date: 1/9/2011
 */

trait NullaryExp[+R] extends Exp[R] {
  override def nodeArity = 0
  def children = Seq()
  def checkedGenericConstructor = _ => this
}

trait UnaryOpTrait[T1 <: Exp[_], +R, Self <: Exp[R]] extends Exp[R] {
  this: Self =>
  def t1: T1
  override def nodeArity = 1
  def children = Seq(t1)
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[T1])
  def copy(t1: T1): Self
}

abstract class UnaryOp[T1 <: Exp[_], +R, Self <: Exp[R]](val t1: T1) extends UnaryOpTrait[T1, R, Self] {
  this: Self =>
}

trait UnaryOpExpTrait[T1, +R, Self <: Exp[R]] extends UnaryOpTrait[Exp[T1], R, Self] {
  this: Self =>
}

abstract class UnaryOpExp[T1, R, Self <: Exp[R]](t1: Exp[T1]) extends UnaryOp[Exp[T1], R, Self](t1) {
  this: Self =>
}


trait Arity2OpExpTrait[T1, T2, +R, Self <: Exp[R]] extends Arity2OpTrait[Exp[T1], Exp[T2], R, Self] {
  this: Self =>
}

//Note: descendents of this class are not necessarily supposed to be commutative, just to have type (A, A) => B.
trait Arity2OpSymmExp[Arg, +R, Self <: Exp[R]] extends Arity2OpExpTrait[Arg, Arg, R, Self] {
  this: Self =>
}
