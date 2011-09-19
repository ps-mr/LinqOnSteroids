package ivm.expressiontree

import annotation.unchecked.uncheckedVariance

// Not exactly sure what I should use to represent applications, but this is the standard App node, well known from
// encodings of the STLC (simply typed lambda calculus).
//Having explicit App nodes for application can be useful to represent application instead of computing it,
//since computing it means inlining and can replicate terms.
case class App[T, U](f: Exp[T => U], t: Exp[T]) extends BinaryOpExp[T => U, T, U](f, t) {
  def interpret = f.interpret()(t.interpret)
  override def copy(f: Exp[T => U], t: Exp[T]) = App(f, t)
}

case class FuncExp[-S, +T](f: Exp[S] => Exp[T]) extends UnaryOpTrait[Exp[T @uncheckedVariance], (S @uncheckedVariance) => (T @uncheckedVariance)] with Equals {
  import FuncExp._
  val x = gensym()
  lazy val body = f(x)
  override def toString() = {
    "(" + x.name + ") => " + body
  }
  //def apply(z: Exp[S]): Exp[T] = f(z) // or rather create App node here?
  //XXX: Uglymost hack. Paolo
  private[ivm] var interpretHook: Option[Exp[Any] => Unit] = None
  def interpret(): S => T =
    z => {
      val res = f(Const(z))
      interpretHook.map(_(res))
      res.interpret()
    }
  private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()
  // @uncheckedVariance here is correct: assume A <: B and consider the following code snippet:
  // val b: FuncExp[S, B] = ...
  // val bA: FuncExp[S, A] = b
  // val e: Exp[A] = ...
  // val res: FuncExp[S, A] = bA.copy(e)
  // Here, res does in fact have the correct type, so this piece of code is type-correct at runtime.
  // Therefore FuncExp[S, A] <: FuncExp[S, B].
  override def copy(t1: Exp[T @uncheckedVariance]): FuncExp[S, T] = makefun(t1, x)
  // The reason is that this copy method could equally be written, weren't it for UnaryOpTrait's signature, like this:
  ///*override*/ def copy[U >: T](t1: Exp[U]): FuncExp[S, U] = makefun(t1, x)
  //But as you can see, the method does not override UnaryOpTrait.copy: override is not accepted there.
  def t1 = body

  // alpha equivalence for functions! (modulo calls to scala functions)
  override def equals(other: Any): Boolean = other match {
     case that: FuncExp[_,_] =>
       val s = gensym()
       that.canEqual(this) && f(s).equals(that.f(s))
     case _ => false
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FuncExp[_,_]]

  // by using varzero, this definition makes sure that alpha-equivalent functions have the same hashcode
  // some non-alpha-equivalent functions will also have the same hash code, such as
  // (x) => (y) => x+y and (x) => (y) => x+x
  // but these functions will not be equal, hence it is only a potential performance problem.
  // Using gensym() here would still give alpha-equivalence, but hashcodes would
  // not be constant!
  override def hashCode() = f(FuncExp.varzero).hashCode()
}

object FuncExp {
  private var varCounter: Int = 0;
  val varzero = gensym()
  def gensym(): Var = { varCounter += 1;  new Var("v"+varCounter) }
  def makefun[S, T](e: Exp[T], v: Var): FuncExp[S, T] = FuncExp(x => e.substVar(v.name,x))
  def makepairfun[S1, S2, T](e: Exp[T], v1: Var, v2: Var): FuncExp[(S1, S2), T] =
    FuncExp(p => e.substVar(v1.name, Proj1(p)).substVar(v2.name, Proj2(p)))
}
