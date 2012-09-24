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
trait LangIntf {
  type Rep[+T]
}

trait BaseLangIntf extends LangIntf {
  //Add a typeclass constraint, instead of ugly tricks to disable the conversion for specific classes.
  //implicit def pure[T](t: T): Rep[T]
  implicit def pure[T: ClassTag: TypeTag](t: T): Rep[T]
}

trait ScalaLangIntf {
  this: LangIntf =>
  //Why not an implicit abstract class? Ah I see.
  implicit def expToNumOps[T: Numeric](t: Rep[T]): NumericOps[T]
  abstract class NumericOps[T: Numeric](t: Rep[T]) {
    def +(that: Rep[T]): Rep[T]
    def *(that: Rep[T]): Rep[T]
    def -(that: Rep[T]): Rep[T]
    def unary_- : Rep[T]
  }
}

trait ConversionDisabler {
  //We forbid implicit conversion from Unit to Exp[Unit] by making it ambiguous. To this end we declare noToExpForUnit.
  //It is more specific than pure[Unit] because it's not generic, but is declared in a superclass, hence
  //has less priority. Ambiguity follows.
  implicit def noToExpForUnit(t: Unit): Exp[Unit] = null
  //Ditto. Creating Const nodes for mutable collection is a contradiction; moreover, those nodes would send no
  //notification for updates to the underlying collection.
  //To test, edit testNoMutableConst below to see that the currently commented-out code does not compile.
  implicit def noConstForMutableColl[T](t: mutable.Traversable[T]): Exp[mutable.Traversable[T]] = null
  implicit def noPureForExp[T](t: Exp[T]): Exp[Exp[T]] = null
}

trait ConversionHelpers {
  def pureExpl[T: ClassTag: TypeTag](t: T): Exp[T] = Const(t)

  //Used to force insertion of the appropriate implicit conversion - unlike ascriptions, one needn't write out the type
  //parameter of Exp here.
  def asExp[T](t: Exp[T]) = t

  //Use something derived from the above to lift other implicit conversions.
  def convLift[T, U](t: Exp[T], name: Symbol, prefix: String)(implicit conv: T => U): Exp[U] =
    new GlobalFuncCall1(name, prefix, conv, t)
  def convFromBase[T <% U, U: ClassTag: TypeTag](t: T): Exp[U] = pureExpl(t: U)

}

//Conversions which should have lower priority than pure.
trait ExtraConversions extends ConversionHelpers {
//  implicit def int2ExpDouble(t: Int) = convFromBase[Int, Double](t)
//  implicit def int2ExpLong(t: Int) = convFromBase[Int, Long](t)
//  implicit def int2ExpFloat(t: Int) = convFromBase[Int, Float](t)
}

trait LiftingConvs extends ConversionDisabler with ExtraConversions with BaseLangIntf {
  type Rep[+T] = Exp[T]
  //The following variant would avoid ugliness like:
  //implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]
  //but it does not work (bug https://issues.scala-lang.org/browse/SI-3346).
  //implicit def pure[T, U <% T](t: U): Exp[T] = Const(t)
  //So let's keep it simple.
  implicit def pure[T: ClassTag: TypeTag](t: T): Exp[T] = pureExpl(t)
  //Failed experiment - allow ignoring calls to pure when they become unneeded. Just a hack.
  //def pure[T: ClassTag: TypeTag](t: Exp[T]): Exp[T] = t
  //Of course, this fails: overloading conversions makes them unavailable as
  //implicit views, so asSmart stops working.

  class WithAsSmartCollection[T](t: T) {
    def asSmart(implicit conv: T => Exp[T]) = conv(t)
  }
}

trait ConversionDisabler2 extends LiftingConvs {
  //Disable conversion - should no more be needed, but it is, as explained below.
  implicit def noAsSmartForExp[T](t: Exp[T]): WithAsSmartCollection[Exp[T]] = null
  /* Assume that `class WithAsSmartCollection[T](t: T)` contains:
def asSmart(implicit conv: T => Exp[T]) = conv(t)
   * (as it does at the moment of this writing).
   * Consider this test: disable the effect of this conversion by using toPimper explicitly:
toPimper(Const(1)).asSmart
   * This code is accepted, while
scala> toPimper(Const(1): Exp[Int]) asSmart
   * gives the expected error, i.e.:
<console>:37: error: ambiguous implicit values:
 both method noPureForExp in trait ConversionDisabler of type [T](t: ivm.expressiontree.Exp[T])ivm.expressiontree.Exp[ivm.expressiontree.Exp[T]]
 and method pure in trait LiftingConvs of type [T](t: T)ivm.expressiontree.Exp[T]
 match expected type ivm.expressiontree.Exp[Int] => ivm.expressiontree.Exp[ivm.expressiontree.Exp[Int]]
              toPimper(Const(1): Exp[Int]) asSmart
   * The compiler is even right, since we do ask for a very specific conversion, from T to Exp[T]; when T = Const[Int], we
   * only have a conversion from U = Exp[Int] to Exp[U] >: Exp[T]. If we write instead:
def asSmart[U >: T](implicit conv: U => Exp[U]): Exp[U] = conv(t)
   * then U might as well be Any; luckily, pure[T] is always available, and noPureForExp will be available as well on expressions.
   * However, the compiler still deduced U = T, so that noPureForExp is not found. The last attempt is this:
def asSmart[U >: T](implicit conv: T => Exp[U]): Exp[U] = conv(t)
   * with the same result - toPimper(Const(1)).asSmart is accepted.
   */
}

trait FunctionOps extends AutoFunctionOps {
  this: LiftingConvs =>
  // Unused!
  def fmap[Res](id: Symbol, callfunc: () => Res) = new Call0(id, Symbol(""), callfunc)

  def globalFmap[A0, Res](t: Exp[A0])
                   (name: Symbol, prefix: String, f: A0 => Res): Exp[Res] =
    new GlobalFuncCall1(name, prefix, f, t)

  def globalFmap[A0, A1, Res](t: Exp[A0], t2: Exp[A1])
                   (name: Symbol, prefix: String, f: (A0, A1) => Res): Exp[Res] =
    new GlobalFuncCall2(name, prefix, f, t, t2)
  /*
  // these functions are explicitly not implicit :)
  def fmap[A0, Res](t: Exp[A0])
                   (name: Symbol, f: A0 => Res, restId: Symbol = Symbol("")): Exp[Res] =
    new Call1(name, restId, f, t)
  def fmap[A0, A1, Res](a0: Exp[A0], a1: Exp[A1])
                       (name: Symbol, f: (A0, A1) => Res, restId: Symbol = Symbol("")): Exp[Res] =
    new Call2(name, restId, f, a0, a1)
  def fmap[A0, A1, A2, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2])
                           (name: Symbol, f: (A0, A1, A2) => Res, restId: Symbol = Symbol("")): Exp[Res] =
    new Call3(name, restId, f, a0, a1, a2)
  def fmap[A0, A1, A2, A3, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3])
                               (name: Symbol, f: (A0, A1, A2, A3) => Res, restId: Symbol = Symbol("")): Exp[Res] =
    new Call4(name, restId, f, a0, a1, a2, a3)
  def fmap[A0, A1, A2, A3, A4, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3], a4: Exp[A4])
                                   (name: Symbol, f: (A0, A1, A2, A3, A4) => Res, restId: Symbol = Symbol("")): Exp[Res] =
    new Call5(name, restId, f, a0, a1, a2, a3, a4)
  */
  /*
  //This is not applied implicitly.
  implicit def liftConv[T, U](t: Exp[T])(implicit tM: Manifest[T], uM: Manifest[U], conv: T => U): Exp[U] =
    fmap(t)(Symbol("liftConv_%s_%s" format (tM.erasure.getName, uM.erasure.getName)), conv)
  //Not even this version is applied implicitly:
  implicit def liftConv[T, U](t: Exp[T])(implicit conv: T => U): Exp[U] = convLift(t, 'liftConvXXX)
  */

  //Should we add this?
  implicit def funcExp[S, T](f: Exp[S] => Exp[T]) = Fun(f)

  implicit def app[A, B](f: Exp[A => B]): Exp[A] => Exp[B] =
    f match {
      //This line should be dropped, but then we'll need to introduce a beta-reducer:
      case fe: Fun[a, b] => fe.f
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

trait ExpProduct {
  //Better name prefix?
  def metaProductArity: Int
  def metaProductElement(n: Int): Exp[Any]
  //The methods below are totally unrelated, they should be pimped instead on Exp[(T1, T2, ..., Tn)].

  //def productArity: Exp[Int]
  //How to implement this? Create the right dynamic node!
  //def productElement(n: Exp[Int]): Exp[Any]
}

trait ExpSelection[TupleT] {
  def body: (Int, Int, Exp[TupleT])
}
//class ExpSelection[TupleT](val arity: Int, val pos: Int, val body: Exp[TupleT])
object ExpSelection {
  def unapply(f: Exp[_]): Option[(Int, Int, Exp[_])] = f match {
    case v: ExpSelection[t] => Some(v.body)
    case _ => None
  }
}

trait TupleOps extends AutoTupleOps {
  this: LiftingConvs =>
  //implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): LiftTuple2[A, B] = LiftTuple2[A, B](pair._1, pair._2)

  //To "unlift" a pair, here's my first solution:
  /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Product2Proj1(pair), Product2Proj2(pair))
  //
  //The second one is just pimp-my-library.
}

trait BaseExps extends LiftingConvs with FunctionOps with TupleOps {
  implicit def toIfInstanceOfOps[T](t: Exp[T]) = new IfInstanceOfOps(t)
  class IfInstanceOfOps[T](t: Exp[T]) {
    def ifInstanceOf[S: ClassTag: TypeTag]: Exp[Option[S]] = IfInstanceOf(t)
  }
}

/**
 * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
 * that implies that the type is obtained through type inference.
 * Instead, we use conversions from T => Exp[T], and
 * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
 */

trait NumOps extends ScalaLangIntf {
  this: LiftingConvs with FunctionOps =>

  class NumericOps[T: Numeric](t: Exp[T]) extends super.NumericOps[T](t) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    def *(that: Exp[T]): Exp[T] = Times(this.t, that)
    def -(that: Exp[T]): Exp[T] = Plus(this.t, Negate(that))
    def unary_- : Exp[T] = Negate(this.t)
  }

  class FractionalOps[T: Fractional](t: Exp[T])(implicit tTag: TypeTag[Fractional[T]]) {
    def /(that: Exp[T]): Exp[T] = fmap(implicitly[Fractional[T]], this.t, that)('FractionalOps$div, _.div(_, _))
  }

  class IntegralOps[T: Integral: TypeTag](t: Exp[T]) {
    def %(that: Exp[T]): Exp[T] = fmap(implicitly[Integral[T]], this.t, that)('IntegralOps$mod, _.rem(_, _))
  }

  //Solution 1:
  //implicit def expToNumOps[T: Numeric, U <% Exp[T]](t: U) = new NumericOps(t)
  //Doesn't work because of https://issues.scala-lang.org/browse/SI-3346 - expToNumOps is the same as mkOps in their example.
  //Solution 2:
  implicit def expToNumOps[T: Numeric](t: Exp[T]) = new NumericOps(t)
  implicit def toNumOps[T: Numeric: ClassTag: TypeTag](t: T) = expToNumOps(t)

  //Same for the others:
  implicit def expToFractionalOps[T: Fractional: TypeTag](t: Exp[T]) = new FractionalOps(t)
  implicit def toFractionalOps[T: Fractional: ClassTag: TypeTag](t: T) = expToFractionalOps(t)

  implicit def expToIntegralOps[T: Integral: TypeTag](t: Exp[T]) = new IntegralOps(t)
  implicit def toIntegralOps[T: Integral: ClassTag: TypeTag](t: T) = expToIntegralOps(t)
}

trait BaseTypesOps {
  this: LiftingConvs with FunctionOps =>

  object expMath {
    def max(a: Exp[Int], b: Exp[Int]) = globalFmap(a, b)('math_max, "math.max", math.max(_, _))
      // new GlobalFuncCall2('math_max, "math.max", (a: Int, b: Int) => math.max(a, b), a, b)
  }

  object expCharacter {
    def isDigit(ch: Exp[Char]) = globalFmap(ch)('math_max, "Character.isDigit", Character isDigit _)
  }

  class OrderingOps[T: Ordering](t: Exp[T]) {
    def <=(that: Exp[T]): Exp[Boolean] = LEq(this.t, that)
    def <(that: Exp[T]): Exp[Boolean] = Less(this.t, that)
    def >(that: Exp[T]): Exp[Boolean] = Less(that, this.t)
    def >=(that: Exp[T]): Exp[Boolean] = LEq(that, this.t)
  }

  //No. Not both operands must be strings, just one is enough.
  class StringOps(t: Exp[String]) {
    def +(that: Exp[String]): Exp[String] = StringConcat(t, that)
    def contains(that: Exp[CharSequence]) = fmap(this.t, that, 'StringOps)('contains, _ contains _)
    def startsWith(that: Exp[String]) = fmap(this.t, that, 'StringOps)('startsWith, _ startsWith _)
    def endsWith(that: Exp[String]) = fmap(this.t, that, 'StringOps)('endsWith, _ endsWith _)
    def indexOf(that: Exp[String]): Exp[Int] = fmap(this.t, that, 'StringOps)('indexOf, _ indexOf _)
    def lastIndexOf(ch: Char): Exp[Int] = lastIndexOf(ch.toInt)
    def lastIndexOf(ch: Exp[Int]): Exp[Int] = fmap(this.t, ch, 'StringOps)('lastIndexOf, _ lastIndexOf _)
    def charAt(idx: Exp[Int]) = fmap(this.t, idx, 'StringOps)('charAt, _ charAt _ )
    def toLowerCase = fmap(this.t, 'StringOps)('toLowerCase, _.toLowerCase)
    def toUpperCase = fmap(this.t, 'StringOps)('toUpperCase, _.toUpperCase)
    def length = fmap(this.t, 'StringOps)('length, _.length)
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
  implicit def toOrderingOps[T: Ordering: ClassTag: TypeTag](t: T) = expToOrderingOps(t)
  implicit def toStringOps(t: String) = expToStringOps(t)
  implicit def toBooleanOps(t: Boolean) = expToBooleanOps(t)
}

trait JavaLibOps {
  this: LiftingConvs with FunctionOps =>

  import java.util.regex.{Pattern, Matcher}
  implicit class PatternOps(t: Exp[Pattern]) {
    def matcher(that: Exp[CharSequence]) = fmap(this.t, that, 'PatternOps)('matcher, _ matcher _)
  }
  implicit def toPatternOps(t: Pattern) = new PatternOps(t)
  implicit class MatcherOps(t: Exp[Matcher]) {
    def find() = fmap(this.t, 'MatcherOps)('find, _.find)
  }
}
