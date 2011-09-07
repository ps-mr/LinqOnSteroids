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

abstract class UnaryOp[T1 <: Exp[_], R](t1: T1) extends CheckingExp[R] {
  override def nodeArity = 1
  def children = Seq(t1)
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[T1])
  def copy(t1: T1): Exp[R]
}

abstract class UnaryOpExp[T1, R](t1: Exp[T1]) extends UnaryOp[Exp[T1], R](t1)

abstract class BinaryOp[T1 <: Exp[_], T2 <: Exp[_], R](t1: T1, t2: T2) extends CheckingExp[R] {
  override def nodeArity = 2
  def children = Seq(t1, t2)
  def checkedGenericConstructor =
    v => copy(v(0).asInstanceOf[T1], v(1).asInstanceOf[T2])
  def copy(t1: T1, t2: T2): Exp[R]
}

abstract class BinaryOpExp[T1, T2, R](t1: Exp[T1], t2: Exp[T2]) extends BinaryOp[Exp[T1], Exp[T2], R](t1, t2)
abstract class BinaryOpSymmExp[Arg, R](t1: Exp[Arg], t2: Exp[Arg]) extends BinaryOpExp[Arg, Arg, R](t1, t2)

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
