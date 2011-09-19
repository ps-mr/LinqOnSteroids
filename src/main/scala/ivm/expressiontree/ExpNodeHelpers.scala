package ivm.expressiontree

/**
 * Here I abstract building blocks for operation nodes, with associated boilerplate. They are especially useful because
 * they encapsulate type-unsafe boilerplate, i.e. the definitions of genericConstructor.
 * User: pgiarrusso
 * Date: 1/9/2011
 */
trait CheckingExp[+R] extends Exp[R] {
  def nodeArity: Int
  protected def checkedGenericConstructor: Seq[Exp[_]] => Exp[R]
  
  def genericConstructor = v =>
    if (v.length == nodeArity)
      checkedGenericConstructor(v)
    else
      throw new IllegalArgumentException()
}

trait NullaryExp[R] extends CheckingExp[R] {
  override def nodeArity = 0
  def children = Seq()
  def checkedGenericConstructor = _ => this
}

trait UnaryOpTrait[T1 <: Exp[_], R] extends CheckingExp[R] {
  def t1: T1
  override def nodeArity = 1
  def children = Seq(t1)
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[T1])
  def copy(t1: T1): Exp[R]
}

abstract class UnaryOp[T1 <: Exp[_], R](val t1: T1) extends UnaryOpTrait[T1, R]

abstract class UnaryOpExp[T1, R](t1: Exp[T1]) extends UnaryOp[Exp[T1], R](t1)

trait BinaryOpTrait[T1 <: Exp[_], T2 <: Exp[_], +R] extends CheckingExp[R] {
  def t1: T1
  def t2: T2
  override def nodeArity = 2
  def children = Seq(t1, t2)
  def checkedGenericConstructor =
    v => copy(v(0).asInstanceOf[T1], v(1).asInstanceOf[T2])
  def copy(t1: T1, t2: T2): Exp[R]
}

abstract class BinaryOp[T1 <: Exp[_], T2 <: Exp[_], +R](val t1: T1, val t2: T2) extends BinaryOpTrait[T1, T2, R]

trait BinaryOpExpTrait[T1, T2, +R] extends BinaryOpTrait[Exp[T1], Exp[T2], R]

// XXX: should this inherit from BinaryOpExpTrait or from BinaryOp? Does it matter? I hope not - these classes should
// stay an implementation detail.
abstract class BinaryOpExp[T1, T2, +R](t1: Exp[T1], t2: Exp[T2]) extends BinaryOp[Exp[T1], Exp[T2], R](t1, t2)
abstract class BinaryOpSymmExp[Arg, +R](t1: Exp[Arg], t2: Exp[Arg]) extends BinaryOpExp[Arg, Arg, R](t1, t2)

//For join
abstract class QuaternaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], R](t1: T1, t2: T2, t3: T3, t4: T4) extends CheckingExp[R] {
  override def nodeArity = 4
  def children = Seq(t1, t2, t3, t4)
  def checkedGenericConstructor =
    v => copy(
      v(0).asInstanceOf[T1],
      v(1).asInstanceOf[T2],
      v(2).asInstanceOf[T3],
      v(3).asInstanceOf[T4])
  def copy(t1: T1, t2: T2, t3: T3, t4: T4): Exp[R]
}

abstract class QuinaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], T5 <: Exp[_], R](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) extends CheckingExp[R] {
  override def nodeArity = 5
  def children = Seq(t1, t2, t3, t4, t5)
  def checkedGenericConstructor =
    v => copy(
      v(0).asInstanceOf[T1],
      v(1).asInstanceOf[T2],
      v(2).asInstanceOf[T3],
      v(3).asInstanceOf[T4],
      v(4).asInstanceOf[T5])
  def copy(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5): Exp[R]
}
