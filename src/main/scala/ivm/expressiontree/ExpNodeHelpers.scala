package ivm.expressiontree

/**
 * Here I abstract building blocks for operation nodes, with associated boilerplate. They are especially useful because
 * they encapsulate type-unsafe boilerplate, i.e. the definitions of genericConstructor.
 * User: pgiarrusso
 * Date: 1/9/2011
 */

trait NullaryExp[R] extends Exp[R] {
  def children = Seq()
  def genericConstructor = _ => this
}

abstract class UnaryOp[T1 <: Exp[_], R](t1: T1) extends Exp[R] {
  def children = Seq(t1)
  def genericConstructor = v => copy(v(0).asInstanceOf[T1])
  def copy(t1: T1): Exp[R]
}

abstract class UnaryOpExp[T1, R](t1: Exp[T1]) extends UnaryOp[Exp[T1], R](t1)

abstract class BinaryOp[T1 <: Exp[_], T2 <: Exp[_], R](t1: T1, t2: T2) extends Exp[R] {
  def children = Seq(t1, t2)
  def genericConstructor = v => copy(v(0).asInstanceOf[T1],
                                       v(1).asInstanceOf[T2])
  def copy(t1: T1, t2: T2): Exp[R]
}

abstract class BinaryOpExp[T1, T2, R](t1: Exp[T1], t2: Exp[T2]) extends BinaryOp[Exp[T1], Exp[T2], R](t1, t2)
abstract class BinaryOpSymmExp[Arg, R](t1: Exp[Arg], t2: Exp[Arg]) extends BinaryOpExp[Arg, Arg, R](t1, t2)

//For join
abstract class QuaternaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], R](t1: T1, t2: T2, t3: T3, t4: T4) extends Exp[R] {
  def children = Seq(t1, t2, t3, t4)
  def genericConstructor =
    v => copy(
      v(0).asInstanceOf[T1],
      v(1).asInstanceOf[T2],
      v(2).asInstanceOf[T3],
      v(3).asInstanceOf[T4])
  def copy(t1: T1, t2: T2, t3: T3, t4: T4): Exp[R]
}

abstract class QuinaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], T5 <: Exp[_], R](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) extends Exp[R] {
  def children = Seq(t1, t2, t3, t4, t5)
  def genericConstructor =
    v => copy(
      v(0).asInstanceOf[T1],
      v(1).asInstanceOf[T2],
      v(2).asInstanceOf[T3],
      v(3).asInstanceOf[T4],
      v(4).asInstanceOf[T5])
  def copy(t1: T1, t2: T2, t3: T3, t4: T4, t5: T5): Exp[R]
}
