package ivm.expressiontree

/**
 * Here I abstract building blocks for operation nodes, with associated boilerplate. They are especially useful because
 * they encapsulate type-unsafe boilerplate, i.e. the definitions of genericConstructor.
 * User: pgiarrusso
 * Date: 1/9/2011
 */

trait Arity0Exp[+R] extends Def[R] {
  override def nodeArity = 0
  def children = Nil
  override protected def checkedGenericConstructor(v: List[Exp[_]]) = this
}

//Note: descendents of this class are not necessarily supposed to be commutative, just to have type (A, A) => B.
trait Arity2OpSymmExp[Arg, +R, Self <: Def[R]] extends Arity2OpExpTrait[Arg, Arg, R, Self] {
  this: Self =>
}
