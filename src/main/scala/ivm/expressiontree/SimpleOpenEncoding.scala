package ivm.expressiontree

import collection.mutable

/**
 * Here I show yet another encoding of expression trees, where methods
 * like +, &lt;= and so on can be added by other classes, rather than having to
 * be inserted in the original object.
 *
 * The original encoding of Klaus had this property but relied on an implicit
 * conversion from T to Exp[T] and then on additional ones from Exp[Double] to
 * DoubleExp, Exp[String] to StringExp, and so on. Quite a few times this did
 * not work because Scala never applies two implicit conversions on top of one
 * another.
 * This can be solved by making their composition available as another implicit conversion, and that's the solution
 * we show here.
 */
trait ConversionDisabler {
  //We forbid implicit conversion from Unit to Exp[Unit] by making it ambiguous. To this end we declare noToExpForUnit.
  //It is more specific than toExp[Unit] because it's not generic, but is declared in a superclass, hence
  //has less priority. Ambiguity follows.
  implicit def noToExpForUnit(t: Unit): Exp[Unit] = null
  //Ditto. Creating Const nodes for mutable collection is a contradiction; moreover, those nodes would send no
  //notification for updates to the underlying collection.
  //To test, edit testNoMutableConst below to see that the currently commented-out code does not compile.
  implicit def noConstForMutableColl[T](t: mutable.Traversable[T]): Exp[mutable.Traversable[T]] = null
}

trait LiftingConvs extends ConversionDisabler {
  implicit def toExp[T](t: T): Exp[T] = Const(t)
  /*implicit def liftOrd[T: Ordering](x: T) = Const(x)
  implicit def liftNum[T: Numeric](x: T) = Const(x)

  implicit def liftBool(x: Boolean): Exp[Boolean] = Const(x)
  implicit def liftString(x: String): Exp[String] = Const(x)*/
}

trait FunctionOps {
  this: LiftingConvs =>
  // these functions are explicitly not implicit :)
  def liftCall[Res](id: Symbol, callfunc: () => Res) = new Call0(id, callfunc)
  def liftCall[A0, Res](id: Symbol, callfunc: A0 => Res, arg0: Exp[A0]) = new Call1(id, callfunc, arg0)
  def liftCall[A0, A1, Res](id: Symbol, callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) =
    new Call2(id, callfunc, arg0, arg1)
  def liftCall[A0, A1, A2, Res](id: Symbol, callfunc: (A0, A1, A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) =
    new Call3(id, callfunc, arg0, arg1, arg2)
  def liftCall[A0, A1, A2, A3, Res](id: Symbol, callfunc: (A0, A1, A2, A3) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3]) =
    new Call4(id, callfunc, arg0, arg1, arg2, arg3)
  def liftCall[A0, A1, A2, A3, A4, Res](id: Symbol, callfunc: (A0, A1, A2, A3, A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) =
    new Call5(id, callfunc, arg0, arg1, arg2, arg3, arg4)

  def onExp[A0, Res](t: Exp[A0])(id: Symbol, f: A0 => Res): Exp[Res] = liftCall(id, f, t)
  def onExp[A0, A1, Res](a0: Exp[A0], a1: Exp[A1])(id: Symbol, f: (A0, A1) => Res): Exp[Res] = liftCall(id, f, a0, a1)
  def onExp[A0, A1, A2, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2])(id: Symbol, f: (A0, A1, A2) => Res): Exp[Res] = liftCall(id, f, a0, a1, a2)
  def onExp[A0, A1, A2, A3, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3])(id: Symbol, f: (A0, A1, A2, A3) => Res): Exp[Res] =
    liftCall(id, f, a0, a1, a2, a3)
  def onExp[A0, A1, A2, A3, A4, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3], a4: Exp[A4])(id: Symbol, f: (A0, A1, A2, A3, A4) => Res): Exp[Res] =
    liftCall(id, f, a0, a1, a2, a3, a4)

  implicit def fToFunOps[A, B](f: Exp[A => B]): Exp[A] => Exp[B] =
    f match {
      case FuncExp(fe) => fe(_) //This line should be dropped, but then we'll need to introduce a beta-reducer.
      // KO: Why do we need a beta-reducer? Since we use HOAS this is just Scala function application
      // and already available in App.interpret
      // But it may still make sense to evaluate such applications right away
      // PG: I believe we need a beta-reducer before any optimization, to ensure that beta-equivalent
      // operations optimize to the same thing. Otherwise the optimizer might not find a pattern
      // because it would show up only after reduction.
      // I believe we want to have App for the same exact reason not all function calls are
      // inlined: preventing code size explosion.
      case _ => App(f, _)
    }

  implicit def expToPartialFunOps[S, T](t: Exp[PartialFunction[S, T]]) = new {
    def isDefinedAt(a: Exp[S]): Exp[Boolean] = IsDefinedAt(t, a)
  }
}

trait TupleOps {
  this: FunctionOps =>
  implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): Pair[A, B] = Pair[A, B](pair._1, pair._2)

  //To "unlift" a pair, here's my first solution:
  /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Proj1(pair), Proj2(pair))
  /*
  //Unfortunately this conversion is not redundant; we may want to have a special node to support this, or to
  //remove Pair constructors applied on top of other pair constructors.
  implicit def expPairToPairExp[A, B](pair: Exp[(A, B)]): Pair[A, B] =
    (Pair[A, B] _).tupled(unliftPair(pair))
  */

  //Here's the second one, adapted from Klaus code. It represents but does not build a tuple (once one adds lazy vals).
  //However, one cannot do pattern matching against the result, not with the existing pattern.
  //Lesson: Scala does not allow to define additional extractors for a given pattern type, and syntax shortcuts such
  //as tuples or => are simply built-in in the language.
  case class PairOps[A, B](p: Exp[(A, B)]) {
    lazy val _1 = Proj1(p)
    lazy val _2 = Proj2(p)
  }

  implicit def toPairHelper[A, B](e: Exp[(A, B)]): PairOps[A, B] = PairOps(e)

  implicit def tripleToTripleExp[A, B, C](triple: (Exp[A], Exp[B], Exp[C])): Exp[(A, B, C)] = onExp(triple._1, triple._2, triple._3)('Tuple3, Tuple3.apply)
  implicit def to3pleHelper[A, B, C](e: Exp[(A, B, C)]) = new {
    def _1 = onExp(e)('_1, _._1)
    def _2 = onExp(e)('_2, _._2)
    def _3 = onExp(e)('_3, _._3)
  }
}

trait BaseExps extends LiftingConvs with FunctionOps with TupleOps {
  //XXX: bad position
  def not(v: Exp[Boolean]) = new NotMaintainerExp(v)
  //XXX: disable IVM, switch back to using the plain Not
  //def not(v: Exp[Boolean]) = Not(v)
}

/**
 * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
 * that implies that the type is obtained through type inference.
 * Instead, we use conversions from T => Exp[T], and
 * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
 */

trait NumOps {
  this: BaseExps =>
  /*class NumOps[T: Numeric](val t: Exp[T]) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
  }
  class OrderingOps[T: Ordering](t: Exp[T]) {
    def <=(that: Exp[T]) = LEq(t, that)
  }

  implicit def expToNumOps[T : Numeric](t: Exp[T]): NumOps[T] = new NumOps(t)
  implicit def tToNumOps[T: Numeric](t: T): NumOps[T] = expToNumOps(t)
  implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
  implicit def tToOrderingOps[T: Ordering](t: T) = expToOrderingOps(t)*/


  class NumericOps[T: Numeric](t: Exp[T]) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    def *(that: Exp[T]): Exp[T] = Times(this.t, that)
    def -(that: Exp[T]): Exp[T] = onExp(implicitly[Numeric[T]], this.t, that)('NumericOps$minus, _.minus(_, _))
  }

  class FractionalOps[T: Fractional](t: Exp[T]) {
    def /(that: Exp[T]): Exp[T] = onExp(implicitly[Fractional[T]], this.t, that)('FractionalOps$div, _.div(_, _))
  }

  class IntegralOps[T: Integral](t: Exp[T]) {
    def %(that: Exp[T]): Exp[T] = onExp(implicitly[Integral[T]], this.t, that)('IntegralOps$mod, _.rem(_, _))
  }

  implicit def expToNumOps[T: Numeric](t: Exp[T]) = new NumericOps(t)
  implicit def expToFractionalOps[T: Fractional](t: Exp[T]) = new FractionalOps(t)
  implicit def expToIntegralOps[T: Integral](t: Exp[T]) = new IntegralOps(t)
  implicit def toNumOps[T: Numeric](t: T) = expToNumOps(t)
  implicit def toFractionalOps[T: Fractional](t: T) = expToFractionalOps(t)
  implicit def toIntegralOps[T: Integral](t: T) = expToIntegralOps(t)

}

