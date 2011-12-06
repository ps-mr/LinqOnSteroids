package ivm.expressiontree

import collection.mutable.HashMap
import actors.threadpool.AtomicInteger

// Not exactly sure what I should use to represent applications, but this is the standard App node, well known from
// encodings of the STLC (simply typed lambda calculus).
//Having explicit App nodes for application can be useful to represent application instead of computing it,
//since computing it means inlining and can replicate terms.
case class App[T, U](f: Exp[T => U], t: Exp[T]) extends BinaryOpExp[T => U, T, U](f, t) {
  def interpret = f.interpret()(t.interpret)
  override def copy(f: Exp[T => U], t: Exp[T]) = App(f, t)
}

abstract class FuncExpBase[-S, +T, +Type] extends Exp[Type] with Equals {
  import FuncExp._
  val f: Exp[S] => Exp[T]

  def xName = x.name
  protected[this] lazy val internX = gensym[S]()
  def x: TypedVar[_] = internX //Since functions are contravariant in S, so TypedVar[S] cannot be a return type for a public method.

  lazy val lazyBody: Exp[T] = f(internX)
  def body: Exp[T] = lazyBody

  override def toString() =
    "(%s) %s %s" format (xName, arrowString, body)
  def arrowString: String

  //Note that thanks to this line and to copy(), we can perform optimization within function bodies, which is notable
  //(even if probably not a contribution)!
  private[ivm] override def children = Seq(body)
  //private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()
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

//The higher-order representation is constructed and passed to FuncExp to share code.
class FuncExpInt[S, T](val foasBody: Exp[T], v: TypedVar[S]) extends FuncExp[S, T](FuncExp.closeOver(foasBody, v)) {
  override def arrowString = "=i=>"

  //The following two lines must be enabled together, I believe:
  override def x = v
  override def body = foasBody

  override def interpret(): S => T =
    z => {
      import FuncExpInt._
      env.get()(v.id) = z
      val res = foasBody.interpret()
      env.get() -= v.id
      res
    }
}
object FuncExpInt {
  private[expressiontree] val env = new ScalaThreadLocal(new HashMap[Int, Any]())
  //Write down a constructor of FuncExpInt from HOAS
  def apply[S, T](f: Exp[S] => Exp[T]) = {
    val v = FuncExp.gensym[S]()
    new FuncExpInt(f(v), v)
  }
}

class ScalaThreadLocal[T](v: => T) extends ThreadLocal[T] {
  override def initialValue() = v
  def modify(f: T => T) {
    set(f(get()))
  }
}

case class IsDefinedAt[S, T](f: Exp[PartialFunction[S, T]], a: Exp[S]) extends BinaryOpExp[PartialFunction[S,T], S, Boolean](f, a) {
  def interpret = f.interpret().isDefinedAt(a.interpret())
  override def copy(f: Exp[PartialFunction[S, T]], a: Exp[S]) = IsDefinedAt(f, a)
}

object FuncExp {
  private val varCounter = new AtomicInteger(0)
  def gensymId(): Int = varCounter.incrementAndGet()
  //def gensym(): Var = Var(gensymId())
  def gensym[T](): TypedVar[T] = TypedVar[T](gensymId())

  val varzero = gensym()

  def closeOver[S, T](e: Exp[T], v: TypedVar[S]): Exp[S] => Exp[T] = x => e.substVar(v.id, x)
  //def makefun[S, T](e: Exp[T], v: Var): FuncExp[S, T] = FuncExp(closeOver(e, v))
  //def makefun[S, T](e: Exp[T], v: Var): FuncExp[S, T] = new FuncExpInt(e, v)
  def makefun[S, T](e: Exp[T], v: TypedVar[/*S*/_]): FuncExp[S, T] = new FuncExpInt(e, v)

  def makePartialFun[S, T](e: Exp[Option[T]], v: TypedVar[S]): PartialFuncExp[S, T] = PartialFuncExp(closeOver(e, v))
  def makepairfun[S1, S2, T](e: Exp[T], v1: TypedVar[/*S1*/_], v2: TypedVar[/*S2*/_]): FuncExp[(S1, S2), T] =
    FuncExp(p => e.substVar(v1.id, Proj1(p)).substVar(v2.id, Proj2(p)))

  //The idea here is similar to normalization-by-evaluation
  def normalize[S, T](f: Exp[S] => Exp[T], x: TypedVar[/*S*/_] = gensym[S]()) = {
    makefun(f(x.asInstanceOf[Exp[S]]), x)
  }
}
