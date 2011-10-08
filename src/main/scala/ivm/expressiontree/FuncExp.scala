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

case class FuncExp[-S, +T](f: Exp[S] => Exp[T]) extends CheckingExp[S => T] with Equals {
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
      interpretHook.map(_(res))  // KO: What is the purpose of this? Remove?
      res.interpret()
    }

  //Copied from UnaryOpTrait {{{
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[T]])
  override def nodeArity = 1
  //}}}

  def copy[U >: T](t1: Exp[U]): FuncExp[S, U] = makefun(t1, x)
  def children = Seq(body)
  private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()

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

// The redundancy between FuncExp and PartialFuncExp should be outsourced into a common superclass

// Note that giving f the type PartialFunction[Exp[S],Exp[T]] would not work, because "definedness"
// can only be determined during interpretation

case class PartialFuncExp[-S, +T](f: Exp[S] => Exp[Option[T]]) extends CheckingExp[PartialFunction[S,T]] with Equals {
  import FuncExp._
  val x = gensym()
  lazy val body = f(x)
  override def toString() = {
    "(" + x.name + ") -(pf)-> " + body
  }
  def interpret(): PartialFunction[S,T] = {
    new PartialFunction[S,T] {
      override def apply(z: S) = {
        f(Const(z)).interpret() match {
          case Some(x) => x
          case _ => sys.error("partial function not defined on arg" + z)
        }
      }
      override def isDefinedAt(z: S) = {
        f(Const(z)).interpret() match {
          case Some(_) => true
          case _ => false
        }
      }
    }
  }

  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[Option[T]]])
  override def nodeArity = 1
  def copy[U >: T](t1: Exp[Option[U]]): PartialFuncExp[S, U] = PartialFuncExp(y => t1.substVar(x.name,y))
  def children = Seq(body)
  private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()

  // alpha equivalence for functions! (modulo calls to scala functions)
  override def equals(other: Any): Boolean = other match {
     case that: PartialFuncExp[_,_] =>
       val s = gensym()
       that.canEqual(this) && f(s).equals(that.f(s))
     case _ => false
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[PartialFuncExp[_,_]]
  override def hashCode() = f(FuncExp.varzero).hashCode()
}

case class IsDefinedAt[S,T](f: Exp[PartialFunction[S,T]], a: Exp[S]) extends BinaryOpExp[PartialFunction[S,T], S, Boolean](f, a){
  def interpret = f.interpret().isDefinedAt(a.interpret())
  override def copy(f: Exp[PartialFunction[S,T]], a: Exp[S]) = IsDefinedAt(f, a)
}

object FuncExp {
  private var varCounter: Int = 0;
  val varzero = gensym()
  def gensym(): Var = { varCounter += 1;  new Var("v"+varCounter) }
  def makefun[S, T](e: Exp[T], v: Var): FuncExp[S, T] = FuncExp(x => e.substVar(v.name,x))
  def makepairfun[S1, S2, T](e: Exp[T], v1: Var, v2: Var): FuncExp[(S1, S2), T] =
    FuncExp(p => e.substVar(v1.name, Proj1(p)).substVar(v2.name, Proj2(p)))
}
