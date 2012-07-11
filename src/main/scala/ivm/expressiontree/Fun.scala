package ivm.expressiontree

import collection.immutable.HashMap
import actors.threadpool.AtomicInteger

//To represent function application, we use the standard App node, well known from
// encodings of the STLC (simply typed lambda calculus).
//Having explicit App nodes for application can be useful to represent application instead of computing it,
//since computing it means inlining and can replicate terms.
case class App[T, U](f: Exp[T => U], t: Exp[T]) extends Arity2OpExp[T => U, T, U, App[T, U]](f, t) with InfixPrinting {
  def interpret() = f.interpret()(t.interpret())
  override def copy(f: Exp[T => U], t: Exp[T]) = App(f, t)
  def operator = "apply"
}

abstract class FuncExpBase[-S, +T, +Type] extends Exp[Type] with Equals {
  import Fun._
  val f: Exp[S] => Exp[T]

  def xName = x.name
  protected[this] lazy val internX = gensym[S]()
  def x: Var = internX //Functions are contravariant in S, so TypedVar[S] cannot be a return type for a public method.

  lazy val lazyBody: Exp[T] = f(internX)
  def body: Exp[T] = lazyBody

  override def toString() =
    "%s %s %s" format (xName, arrowString, body)
  def arrowString: String

  //Note that thanks to this line and to copy(), we can perform optimization within function bodies, which is notable
  //(even if probably not a contribution)!
  /*private[ivm]*/ override def children = Seq(body)
  //private[ivm] override def closedTermChildren: Seq[Exp[_]] = Seq()
  //Copied from Arity1OpTrait:
  override def nodeArity = 1

  // by using varzero, this definition makes sure that alpha-equivalent functions have the same hashcode
  // some non-alpha-equivalent functions will also have the same hash code, such as
  // (x) => (y) => x+y and (x) => (y) => x+x
  // but these functions will not be equal, hence it is only a potential performance problem.
  // Using gensym() here would still give alpha-equivalence, but hashcodes would
  // not be constant!
  override def hashCode() = f(Fun.varzero).hashCode()

  // alpha equivalence for functions! (modulo calls to scala functions)
  override def equals(other: Any): Boolean = other match {
     case that: FuncExpBase[_, _, _] =>
       val s = gensym()
       (this canEqual that) && //This is a hack for testing that.isInstanceOf[Foo]
         (that canEqual this) && (this.f(s) == that.f(s))
     case _ => false
  }
}

abstract class Fun[-S, +T](val f: Exp[S] => Exp[T]) extends FuncExpBase[S, T, S => T] {
  def interpret(): S => T =
    z => f(Const(z)).interpret()

  def copy[U >: T](t1: Exp[U]): Fun[S, U] = Fun.makefun(t1, x)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Fun[_,_]]
  //Copied from Arity1OpTrait:
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[T]])
}

// Note that giving f the type PartialFunction[Exp[S],Exp[T]] would not work, because "definedness"
// can only be determined during interpretation

case class PartialFuncExp[-S, +T](f: Exp[S] => Exp[Option[T]]) extends FuncExpBase[S, Option[T], PartialFunction[S, T]] {
  def interpret(): PartialFunction[S,T] =
    Function.unlift(Fun(f).interpret())

  def arrowString = "=(pf)=>"
  def copy[U >: T](t1: Exp[Option[U]]): PartialFuncExp[S, U] = Fun.makePartialFun(t1, x)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[PartialFuncExp[_,_]]
  //Copied from Arity1OpTrait:
  def checkedGenericConstructor = v => copy(v(0).asInstanceOf[Exp[Option[T]]])
  override def toCode = "Function.unlift(%s)" format (Fun(f).toCode)
}

//The higher-order representation is constructed and passed to Fun to share code.
class FunInterp[S, T](val foasBody: Exp[T], v: TypedVar[S]) extends Fun[S, T](Fun.toHOAS(foasBody, v)) {
  override def arrowString = "=>"

  //The following two overrides must be either both present or both absent. Without this override, the body would be
  //recomputed using substitution.
  override def x = v
  override def body = foasBody

  override def interpret(): S => T = {
    //Close over the current environment, and ensure it is stored in the returned closure.
    val env = FunInterp.env.get()
    z => FunInterp.env.withValue(env + (v.id -> z))(foasBody.interpret())
    /*
    // This is broken when more functions are nested. Bad things can happen if res is a Scala View which will
    // still execute an interpreted function, which references an environment. We are not constructing closures, but
    // using dynamic scoping!!
    z => {
      import FunInterp._
      env.get()(v.id) = z
      val res = foasBody.interpret()
      env.get() -= v.id
      res
    }
    */
  }
  override def toCode = "%s => %s" format (v.toCode, body.toCode)
}

object FunInterp {
  private[expressiontree] val env = new ScalaThreadLocal[Map[Int, Any]](new HashMap[Int, Any]())
}

class FunInterp2[S1, S2, T](val foasBody: Exp[T], v1: TypedVar[S1], v2: TypedVar[S2])
  extends Fun[(S1, S2), T](p => foasBody.substSubTerm(v1, Tuple2Proj1(p)).substSubTerm(v2, Tuple2Proj2(p)))
{
  override def arrowString = "=i=>"
  override def interpret() = {
    //Close over the current environment, and ensure it is stored in the returned closure.
    val env = FunInterp.env.get()
    //Apparently, on case functions the type annotation must be on the immediately enclosing expression - putting it as
    //the return value of interpret() does not work. Workaround this bug this way.
    val interpFun: ((S1, S2)) => T =
    {
      case (z1, z2) =>
        FunInterp.env.withValue(env + (v1.id -> z1) + (v2.id -> z2))(foasBody.interpret())
    }
    interpFun
  }
  override def toCode = "(%s, %s) => %s" format (v1.toCode, v2.toCode, body.toCode)
}

class ScalaThreadLocal[T](v: => T) extends ThreadLocal[T] {
  override def initialValue() = v
  def modify(f: T => T) {
    set(f(get()))
  }
  def withValue[U](tempV: T)(toCompute: => U) = {
    val old = get()

    set(tempV)
    val res = toCompute
    set(old)
    res
  }
}

case class IsDefinedAt[S, T](f: Exp[PartialFunction[S, T]], a: Exp[S]) extends Arity2OpExp[PartialFunction[S,T], S, Boolean, IsDefinedAt[S, T]](f, a) {
  def interpret() = f.interpret().isDefinedAt(a.interpret())
  override def copy(f: Exp[PartialFunction[S, T]], a: Exp[S]) = IsDefinedAt(f, a)
}

object Fun {
  //Constructs FunInterp from HOAS. This also applies normalization-by-evaluation in the process.
  def toFOAS[S, T](funBody: Exp[S] => Exp[T]): FunInterp[S, T] = {
    val v = Fun.gensym[S]()
    new FunInterp(funBody(v), v)
  }

  //Force switch to FunInterp everywhere with a single line of code :-)
  //XXX: We should hide the use of FunInterp, but a few clients use this detail; FunInterp.x has a more precise type.
  def apply[S, T](f: Exp[S] => Exp[T]): FunInterp[S, T] = toFOAS(f)
  def unapply(f: Fun[_, _]) = Some(f.f)

  def rename[S, T](f: Fun[S, T]): FunInterp[S, T] = {
    toFOAS(f.f)
  }

  private val varCounter = new AtomicInteger(0)
  def gensymId(): Int = varCounter.incrementAndGet()
  //def gensym(): Var = Var(gensymId())
  def gensym[T](): TypedVar[T] = TypedVar[T](gensymId())

  val varzero = gensym()

  def toHOAS[S, T](openTerm: Exp[T], v: TypedVar[S]): Exp[S] => Exp[T] = x => openTerm.substSubTerm(v, x)
  //def makefun[S, T](e: Exp[T], v: Var): Fun[S, T] = Fun(toHOAS(e, v))
  //def makefun[S, T](e: Exp[T], v: Var): Fun[S, T] = new FunInterp(e, v)
  def makefun[S, T](e: Exp[T], v: TypedVar[/*S*/_]): Fun[S, T] = new FunInterp(e, v)

  def makePartialFun[S, T](e: Exp[Option[T]], v: TypedVar[S]): PartialFuncExp[S, T] = PartialFuncExp(toHOAS(e, v))
  def makepairfun[S1, S2, T](e: Exp[T], v1: TypedVar[/*S1*/_], v2: TypedVar[/*S2*/_]): Fun[(S1, S2), T] = {
    //This implementation is correct but slow!
    //Fun(p => e.substSubTerm(v1, Tuple2Proj1(p)).substSubTerm(v2, Tuple2Proj2(p)))
    //This implementation is correct but limits further optimizations since it uses the opaque fmap
    /*Fun(arg =>
      App(
        Lifting.fmap(new FunInterp[Any, (Any => T)](new FunInterp(e, v1), v2))('tupledCurried, x => Function.tupled(Function.uncurried(x))),
        arg))*/
    new FunInterp2(e, v1, v2)
  }
}
