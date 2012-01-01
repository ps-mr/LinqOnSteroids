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

trait UnaryOpTrait[T1 <: Exp[_], +R] extends Exp[R] {
  def t1: T1
  override def nodeArity = 1
  def children = Seq(t1)
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[T1])
  def copy(t1: T1): Exp[R]
}

abstract class UnaryOp[T1 <: Exp[_], +R](val t1: T1) extends UnaryOpTrait[T1, R]

abstract class UnaryOpExp[T1, R](t1: Exp[T1]) extends UnaryOp[Exp[T1], R](t1)

trait BinaryOpTrait[T1 <: Exp[_], T2 <: Exp[_], +R] extends Exp[R] {
  def t1: T1
  def t2: T2
  override def nodeArity = 2
  def children = Seq(t1, t2)
  def checkedGenericConstructor =
    v => copy(v(0).asInstanceOf[T1], v(1).asInstanceOf[T2])
  def copy(t1: T1, t2: T2): Exp[R]
}

//XXX: replace BinaryOpTrait with this, to prevent statically the only boilerplate-related copy-n-paste bug in defining tree classes.
trait BinaryOpTrait2[T1 <: Exp[_], T2 <: Exp[_], +R, Self <: Exp[R]] extends BinaryOpTrait[T1, T2, R] {
  //Ensure that Self is actually correct:
  this: Self =>
  //Ensure that copy returns the correct type.
  override def copy(t1: T1, t2: T2): Self
}

abstract class BinaryOp[T1 <: Exp[_], T2 <: Exp[_], +R](val t1: T1, val t2: T2) extends BinaryOpTrait[T1, T2, R]

trait BinaryOpExpTrait[T1, T2, +R] extends BinaryOpTrait[Exp[T1], Exp[T2], R]

// Should this inherit from BinaryOpExpTrait or from BinaryOp? Does it matter? I hope not - these classes should
// stay an implementation detail.
abstract class BinaryOpExp[T1, T2, +R](t1: Exp[T1], t2: Exp[T2]) extends BinaryOp[Exp[T1], Exp[T2], R](t1, t2)
abstract class BinaryOpSymmExp[Arg, +R](t1: Exp[Arg], t2: Exp[Arg]) extends BinaryOpExp[Arg, Arg, R](t1, t2)

trait TernaryOpTrait[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], +R] extends Exp[R] {
  def t1: T1
  def t2: T2
  def t3: T3
  override def nodeArity = 3
  def children = Seq(t1, t2, t3)
  def checkedGenericConstructor =
    v => copy(v(0).asInstanceOf[T1], v(1).asInstanceOf[T2], v(2).asInstanceOf[T3])
  def copy(t1: T1, t2: T2, t3: T3): Exp[R]
}
abstract class TernaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], +R](val t1: T1, val t2: T2, val t3: T3)

  extends TernaryOpTrait[T1, T2, T3, R]

abstract class TernaryOpExp[T1, T2, T3, +R](t1: Exp[T1], t2: Exp[T2], t3: Exp[T3])
  extends TernaryOp[Exp[T1], Exp[T2], Exp[T3], R](t1, t2, t3)

//For Call4
abstract class QuaternaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], +R](t1: T1, t2: T2, t3: T3, t4: T4)
                                                                                        extends Exp[R] {
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

abstract class QuaternaryOpExp[T1, T2, T3, T4, +R](t1: Exp[T1], t2: Exp[T2], t3: Exp[T3], t4: Exp[T4])

  extends QuaternaryOp[Exp[T1], Exp[T2], Exp[T3], Exp[T4], R](t1, t2, t3, t4)

//For Join and Call5
abstract class QuinaryOp[T1 <: Exp[_], T2 <: Exp[_], T3 <: Exp[_], T4 <: Exp[_], T5 <: Exp[_], +R](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5)
                                                                                                  extends Exp[R] {
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
abstract class QuinaryOpExp[T1, T2, T3, T4, T5, +R](t1: Exp[T1], t2: Exp[T2], t3: Exp[T3], t4: Exp[T4], t5: Exp[T5])

  extends QuinaryOp[Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], R](t1, t2, t3, t4, t5)

