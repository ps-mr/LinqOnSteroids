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
  //The following variant would avoid ugliness like:
  //implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]
  //but it does not work (bug https://issues.scala-lang.org/browse/SI-3346).
  //implicit def toExp[T, U <% T](t: U): Exp[T] = Const(t)
  //So let's keep it simple.
  implicit def toExp[T](t: T): Exp[T] = Const(t)

  //Used to force insertion of the appropriate implicit conversion - unlike ascriptions, one needn't write out the type
  //parameter of Exp here.
  def asExp[T](t: Exp[T]) = t

  class WithAsSmartCollection[T](t: T) {
    def asSmartCollection = asExp(t)
  }
}

trait ConversionDisabler2 extends LiftingConvs {
  //Disable conversion
  implicit def noAsSmartCollForExp[T](t: Exp[T]): WithAsSmartCollection[Exp[T]] = null
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
      case FuncExp(fe) => fe.asInstanceOf[Exp[A] => Exp[B]] //This line should be dropped, but then we'll need to introduce a beta-reducer.
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

  class PartialFunctionOps[S, T](t: Exp[PartialFunction[S, T]]) {
    def isDefinedAt(a: Exp[S]): Exp[Boolean] = IsDefinedAt(t, a)
  }

  implicit def expToPartialFunOps[S, T](t: Exp[PartialFunction[S, T]]) = new PartialFunctionOps(t)
}

trait TupleOps extends AutoTupleOps {
  this: FunctionOps =>
  //implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): LiftTuple2[A, B] = LiftTuple2[A, B](pair._1, pair._2)

  //To "unlift" a pair, here's my first solution:
  /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Tuple2Proj1(pair), Tuple2Proj2(pair))
  //
  //The second one is just pimpl-my-library.
}

trait BaseExps extends LiftingConvs with FunctionOps with TupleOps {
  implicit def toIfInstanceOfOps[T](t: Exp[T]) = new toIfInstanceOfOps(t)
  class toIfInstanceOfOps[T](t: Exp[T]) {
    def ifInstanceOf[S](implicit cS: ClassManifest[S]): Exp[Option[S]] = IfInstanceOf(t, cS)
  }
}

/**
 * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
 * that implies that the type is obtained through type inference.
 * Instead, we use conversions from T => Exp[T], and
 * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
 */

trait NumOps {
  this: LiftingConvs with FunctionOps =>

  class NumericOps[T: Numeric](t: Exp[T]) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    def *(that: Exp[T]): Exp[T] = Times(this.t, that)
    def -(that: Exp[T]): Exp[T] = Plus(this.t, Negate(that))
    def unary_- : Exp[T] = Negate(this.t)
  }

  class FractionalOps[T: Fractional](t: Exp[T]) {
    def /(that: Exp[T]): Exp[T] = onExp(implicitly[Fractional[T]], this.t, that)('FractionalOps$div, _.div(_, _))
  }

  class IntegralOps[T: Integral](t: Exp[T]) {
    def %(that: Exp[T]): Exp[T] = onExp(implicitly[Integral[T]], this.t, that)('IntegralOps$mod, _.rem(_, _))
  }

  //Solution 1:
  //implicit def expToNumOps[T: Numeric, U <% Exp[T]](t: U) = new NumericOps(t)
  //Doesn't work because of https://issues.scala-lang.org/browse/SI-3346 - expToNumOps is the same as mkOps in their example.
  //Solution 2:
  implicit def expToNumOps[T: Numeric](t: Exp[T]) = new NumericOps(t)
  implicit def toNumOps[T: Numeric](t: T) = expToNumOps(t)

  //Same for the others:
  implicit def expToFractionalOps[T: Fractional](t: Exp[T]) = new FractionalOps(t)
  implicit def toFractionalOps[T: Fractional](t: T) = expToFractionalOps(t)

  implicit def expToIntegralOps[T: Integral](t: Exp[T]) = new IntegralOps(t)
  implicit def toIntegralOps[T: Integral](t: T) = expToIntegralOps(t)
}

trait BaseTypesOps {
  this: LiftingConvs with FunctionOps =>

  class OrderingOps[T: Ordering](t: Exp[T]) {
    def <=(that: Exp[T]): Exp[Boolean] = LEq(this.t, that)
    def <(that: Exp[T]): Exp[Boolean] = Less(this.t, that)
    def >(that: Exp[T]): Exp[Boolean] = Less(that, this.t)
    def >=(that: Exp[T]): Exp[Boolean] = LEq(that, this.t)
  }

  class StringOps(t: Exp[String]) {
    def +(that: Exp[String]) = StringConcat(t, that)
  }

  class BooleanOps(b: Exp[Boolean]) {
    def &&(that: Exp[Boolean]) = And(b, that)
    def ||(that: Exp[Boolean]) = Or(b, that)
    def unary_! = Not(b)
  }

  implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
  implicit def expToStringOps(t: Exp[String]) = new StringOps(t)
  implicit def expToBooleanOps(t: Exp[Boolean]) = new BooleanOps(t)

  /*
   * In these definitions of toNumOps and toOrderingOps, implicit resolution fails because of ambiguity between liftOrd
   * and liftNum, if they are both declared - even if the ambiguity could easily be solved. The problem can be solved by
   * just having a polymorphic lift conversion. Other solutions are possible here but don't remove this ambiguity that
   * affects client code then.
   */
  implicit def toOrderingOps[T: Ordering](t: T) = expToOrderingOps(t)
  implicit def toStringOps(t: String) = expToStringOps(t)
  implicit def toBooleanOps(t: Boolean) = expToBooleanOps(t)
}
