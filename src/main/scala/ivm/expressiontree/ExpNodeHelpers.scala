package ivm.expressiontree

/**
 * Here I abstract building blocks for operation nodes, with associated boilerplate. They are especially useful because
 * they encapsulate type-unsafe boilerplate, i.e. the definitions of genericConstructor.
 * User: pgiarrusso
 * Date: 1/9/2011
 */

trait Arity0Exp[+R] extends Exp[R] {
  override def nodeArity = 0
  def children = Seq()
  def checkedGenericConstructor = _ => this
}

trait Arity1OpExpTrait[T1, +R, Self <: Exp[R]] extends Arity1OpTrait[Exp[T1], R, Self] {
  this: Self =>
}


trait Arity2OpExpTrait[T1, T2, +R, Self <: Exp[R]] extends Arity2OpTrait[Exp[T1], Exp[T2], R, Self] {
  this: Self =>
}

//Note: descendents of this class are not necessarily supposed to be commutative, just to have type (A, A) => B.
trait Arity2OpSymmExp[Arg, +R, Self <: Exp[R]] extends Arity2OpExpTrait[Arg, Arg, R, Self] {
  this: Self =>
}
