package ivm.expressiontree

// Not exactly sure what I should use to represent applications, but this is the standard App node, well known from
// encodings of the STLC (simply typed lambda calculus).
//Having explicit App nodes for application can be useful to represent application instead of computing it,
//since computing it means inlining and can replicate terms.
case class App[T, U](f: Exp[T => U], t: Exp[T]) extends BinaryOpExp[T => U, T, U](f, t) {
  def interpret = f.interpret()(t.interpret)
  override def copy(f: Exp[T => U], t: Exp[T]) = App(f, t)
}

abstract class FuncExpBase[-S, +T, +Type] extends CheckingExp[Type] with Equals {
  import FuncExp._
  val f: Exp[S] => Exp[T]

  def xName = x.name
  lazy val x = gensym()
  lazy val body = f(x)
  override def toString() =
    "(%s) %s %s" format (xName, arrowString, body)
  def arrowString: String

  private[ivm] override def children = Seq(body)
  private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()
  //Copied from UnaryOpTrait:
  override def nodeArity = 1

  // by using varzero, this definition makes sure that alpha-equivalent functions have the same hashcode
  // some non-alpha-equivalent functions will also have the same hash code, such as
  // (x) => (y) => x+y and (x) => (y) => x+x
  // but these functions will not be equal, hence it is only a potential performance problem.
  // Using gensym() here would still give alpha-equivalence, but hashcodes would
  // not be constant!
  override def hashCode() = f(FuncExp.varzero).hashCode()

  // alpha equivalence for functions! (modulo calls to scala functions)
  override def equals(other: Any): Boolean = other match {
     case that: FuncExpBase[_, _, _] =>
       val s = gensym()
       (this canEqual that) && //This is a hack for testing that.isInstanceOf[Foo]
         (that canEqual this) && (this.f(s) == that.f(s))
     case _ => false
  }
}

case class FuncExp[-S, +T](f: Exp[S] => Exp[T]) extends FuncExpBase[S, T, S => T] {
  def interpret(): S => T =
    z => f(Const(z)).interpret()

  def arrowString = "=>"
  def copy[U >: T](t1: Exp[U]): FuncExp[S, U] = FuncExp.makefun(t1, x)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FuncExp[_,_]]
  //Copied from UnaryOpTrait:
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[T]])
}

// Note that giving f the type PartialFunction[Exp[S],Exp[T]] would not work, because "definedness"
// can only be determined during interpretation

case class PartialFuncExp[-S, +T](f: Exp[S] => Exp[Option[T]]) extends FuncExpBase[S, Option[T], PartialFunction[S, T]] {
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

  def arrowString = "-(pf)->"
  def copy[U >: T](t1: Exp[Option[U]]): PartialFuncExp[S, U] = FuncExp.makePartialFun(t1, x)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[PartialFuncExp[_,_]]
  //Copied from UnaryOpTrait:
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[Option[T]]])
}

case class IsDefinedAt[S,T](f: Exp[PartialFunction[S,T]], a: Exp[S]) extends BinaryOpExp[PartialFunction[S,T], S, Boolean](f, a){
  def interpret = f.interpret().isDefinedAt(a.interpret())
  override def copy(f: Exp[PartialFunction[S,T]], a: Exp[S]) = IsDefinedAt(f, a)
}

object FuncExp {
  private var varCounter: Int = 0;
  val varzero = gensym()
  def gensymId() = { varCounter += 1; varCounter }
  def gensym(): Var = Var(gensymId())
  def closeOver[S, T](e: Exp[T], v: Var): Exp[S] => Exp[T] = x => e.substVar(v.id, x)
  def makefun[S, T](e: Exp[T], v: Var): FuncExp[S, T] = FuncExp(closeOver(e, v))
  def makePartialFun[S, T](e: Exp[Option[T]], v: Var): PartialFuncExp[S, T] = PartialFuncExp(closeOver(e, v))
  def makepairfun[S1, S2, T](e: Exp[T], v1: Var, v2: Var): FuncExp[(S1, S2), T] =
    FuncExp(p => e.substVar(v1.id, Proj1(p)).substVar(v2.id, Proj2(p)))
}
