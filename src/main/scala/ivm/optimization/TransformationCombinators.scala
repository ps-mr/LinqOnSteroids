package ivm
package optimization

import scalaz._
import Scalaz._
import runtime.AbstractPartialFunction

/**
 * User: pgiarrusso
 * Date: 20/4/2012
 */

object TransformationCombinatorsExperiments {
  import expressiontree.Exp
  //TODO: For M[_] = Option, add overloads for PartialFunctions.
  class TransformationCombinatorsScalaz[T] {
    type TransformerFun[M[_]] = T => M[T]
    // This implicit conversion makes crazy Scalaz methods (like >=>) available on TransformerFun.
    implicit def toKeisli[M[_]](f: TransformerFun[M]): Kleisli[M, T, T] = kleisli(f)

    abstract class Transformer[M[_]] extends TransformerFun[M] {
      //XXX: this definition of map is very different from Parser combinator's map or ^^ operator.
      def map(q: => T => T)(implicit m: Functor[M]): Transformer[M] = {
        lazy val q0 = q
        Transformer { kleisli(this) map q0 }
      }

      //To implement ^^, we need to change the used monad.
      def compose[N[_]](f: M[T] => N[T]): Transformer[N] = Transformer { this compose f }
      def &(q: => Transformer[M])(implicit m: Bind[M]): Transformer[M] = {
        lazy val q0 = q
        Transformer { kleisli(this) >=> q0 }
      }
      def |(q: => Transformer[M])(implicit plus: Plus[M]): Transformer[M] = {
        lazy val q0 = q
        Transformer { in => plus.plus(this(in), q0(in)) }
      }
      def *(implicit m: Monad[M], p: Plus[M]) = rep(this)
    }

    def Transformer[M[_]](f: => TransformerFun[M]) = {
      lazy val f0 = f
      new Transformer[M] {
        def apply(in: T) = f0(in)
      }
    }

    def emptyTransform[M[_]](implicit m: Pure[M]): Transformer[M] = Transformer[M] { m.pure(_) }
    //This method is not at all tail-recursive...
    def kleeneStar[M[_]: Monad: Plus](f: => Transformer[M]): Transformer[M] =
      f & kleeneStar(f) | emptyTransform

    //...hence "tie the knot" explicitly. TODO: test that this is actually beneficial.
    def rep[M[_]: Monad: Plus](f: => Transformer[M]): Transformer[M] = {
      def resultFun: Transformer[M] = Transformer { f & resultFun | emptyTransform }
      resultFun
    }
  }

  class ParserCombinatorFromTransformerCombinator[T] extends TransformationCombinatorsScalaz[T] {
    //XXX Simplify
    //Note that we pass just a functor to super.Transformer, not a monad. This would be however a writer monad if U
    //were a monoid. OTOH, parser combinators work without requiring an actual monad - sequencing in parser combinators
    //relies on a non-associative pairing operation.
    abstract class Transformer[U] extends super.Transformer[({type l[a] = (a, U)})#l] {
      //We need to emulate virtual classes - here we get super.Transformer as the result type. See the emulation of virtual
      // classes in the Scala implementation of views.
      def ^^[V](f: U => V): ParserCombinatorFromTransformerCombinator.super.Transformer[({type l[a] = (a, V)})#l] = super.compose[({type l[a] = (a, V)})#l] {(_: (T, U)/* ({type l[a] = (a, U)})#l[T]*/) map f}
    }
  }

  class TransformationCombinators {
    type TransformerOpt = Exp[_] => Option[Exp[_]]
    abstract class Transformer3 extends TransformerOpt {
      def &(q: => TransformerOpt): TransformerOpt = {
        lazy val q0 = q
        Transformer3 { kleisli(this) >=> q0 }
      }
      def |(q: => TransformerOpt): TransformerOpt = {
        lazy val q0 = q
        Transformer3 { in => this(in) orElse q0(in) }
      }
    }
    def Transformer3(f: TransformerOpt) = new Transformer3 {
      def apply(in: Exp[_]) = f(in)
    }

    def test(f: TransformerOpt) = kleisli(f)
    def compose(f: TransformerOpt, g: TransformerOpt) = kleisli(f) >=> g
    implicitly[Monad[Option]]
    implicitly[Monoid[Endo[Exp[_]]]](Monoid.monoid(Semigroup.EndoSemigroup, Zero.EndoZero))
    Category.KleisliCategory[Option] //Now we need to convert this to a monoid. Actually we don't - we only want to
    // abstract on suitable Kleisli categories of Monads.
  }
  object TransformationCombinators extends TransformationCombinators /*with App*/ {
    import OptimizationTransforms.{deltaReductionTuple, betaReduction}

    object Foo extends TransformationCombinatorsScalaz[Exp[_]]
    def betaDeltaReducer3 = {
      import Foo._
      Function.unlift((Transformer {(deltaReductionTuple orElse betaReduction).lift}).*)
    }
  }
}

trait SimplePartialFunction[-T1, R] extends Function1[T1, R] with PartialFunction[T1, R] { self =>
  def simpleApplyOrElse[A1 <: T1](x: A1, default: A1 => R): R
  def apply(x: T1): R = simpleApplyOrElse(x, PartialFunction.empty)
}

class TransformationCombinators[Exp[_]] {
  //implicitly[C]
  //implicitly[Endo[Exp[_]]]
  //implicit val m: Monoid[PartialFunction[Exp[_], Exp[_]]]
  type TransformerBase = PartialFunction[Exp[_], Exp[_]]
  trait Transformer[T] extends PartialFunction[Exp[T], Exp[T]] {
    self =>
    //def on[T](in: Exp[T]): Exp[T] = apply(in).asInstanceOf[Exp[T]]
    def on(in: Exp[T]): Exp[T] = apply(in)
    def &(q: => (Exp[T] => Exp[T])): Transformer[T] = {
      lazy val q0 = q
      //this andThen q0 //Eta-expansion should be applied _here_
      //TransformerT { case in if isDefinedAt(in) => q0(this(in)) }

      /*
      TransformerT (new SimplePartialFunction[Exp[T], Exp[T]] {
        //On such a partial function, invoking applyOrElse goes through two steps. Hm.
        override def simpleApplyOrElse[A1 <: Exp[T]](x: A1, default: A1 => Exp[T]): Exp[T] =
          q0(self applyOrElse (x, default))
        override def isDefinedAt(x: Exp[T]) = self isDefinedAt x
      })
      */
      TransformerT (new AbstractPartialFunction[Exp[T], Exp[T]] {
        override def applyOrElse[A1 <: Exp[T], B1 >: Exp[T]](x: A1, default: A1 => B1): B1 =
          self andThen q0 applyOrElse (x, default)
        override def isDefinedAt(x: Exp[T]) = self isDefinedAt x
      })
    }
    def |(q: => Transformer[T]): Transformer[T] = {
      lazy val q0 = q
      //this orElse q0 //Eta-expansion should be applied _here_
      //Transformer {case in if !isDefinedAt(in) => q0(in)} //buggy
      TransformerT (new AbstractPartialFunction[Exp[T], Exp[T]] {
        override def applyOrElse[A1 <: Exp[T], B1 >: Exp[T]](x: A1, default: A1 => B1): B1 =
          self applyOrElse (x, (x: A1) => q0 applyOrElse (x, default))
        override def isDefinedAt(x: Exp[T]) = (self isDefinedAt x) || (q0 isDefinedAt x)
      })
      /*case in if isDefinedAt(in) => this(in)
        case in if !isDefinedAt(in) => q0(in)
      }*/
    }
  }
  /*def Transformer(f: TransformerOld) = new Transformer {
    def apply(in: Exp[_]) = f(in)
    def isDefinedAt(x: Exp[_]) = f.isDefinedAt(x)
  }*/
  def Transformer[T](f: TransformerBase) = TransformerT(f.asInstanceOf[PartialFunction[Exp[T], Exp[T]]])

  def TransformerT[T](f: PartialFunction[Exp[T], Exp[T]]) = new AbstractPartialFunction[Exp[T], Exp[T]] with Transformer[T] {
    override def applyOrElse[A1 <: Exp[T], B1 >: Exp[T]](x: A1, default: A1 => B1): B1 =
      f.applyOrElse(x, default)
    override def isDefinedAt(x: Exp[T]) = f.isDefinedAt(x)
  }

  //This is the shortest way of writing identity.
  val emptyTransformOld: TransformerBase = {case e => e}

  def emptyTransform[T]: Transformer[T] = Transformer(emptyTransformOld)
  def kleeneStar[T](f: => Transformer[T]): Exp[T] => Exp[T] =
    f & kleeneStar(f) orElse emptyTransform

  def fromPoly(e: Exp[Any] => Exp[Any]) = e

  def kleeneStarOld(f: TransformerBase): Exp[_] => Exp[_] = {
    def resultFun(exp: Exp[_]): Exp[_] = (f andThen resultFun orElse emptyTransformOld)(exp)
    resultFun _
    //def resultFun: Exp[_] => Exp[_] = (f andThen resultFun orElse emptyTransform)
    //resultFun
  }
  /*
  //Ideal definition, maybe - this should at least work, if not very efficiently because of the lack of sharing:
  def kleeneStar(f: => Transformer): Exp[_] => Exp[_] =
    f andThen kleeneStar(f) orElse emptyTransform
  */
}

object TransformationCombinators extends TransformationCombinators[expressiontree.Exp]
