package ivm
package optimization

import scalaz._
import Scalaz._
import tests.Debug

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

class TransformationCombinators[Exp[_]] {
  def debugPrintln[T](x: T) = if (Debug.active) Console.println(x)
  import runtime.AbstractPartialFunction

  //implicitly[C]
  //implicitly[Endo[Exp[_]]]
  //implicit val m: Monoid[PartialFunction[Exp[_], Exp[_]]]
  type TransformerBase = PartialFunction[Exp[_], Exp[_]]
  trait Transformer[T] extends PartialFunction[Exp[T], Exp[T]] {
    self =>
    //def on[T](in: Exp[T]): Exp[T] = apply(in).asInstanceOf[Exp[T]]
    def on(in: Exp[T]): Exp[T] = apply(in)
    def &(q: => (Exp[T] => Exp[T])): Transformer[T] = {
      debugPrintln("& evaluated!")
      TransformerT(this andThen q, Some("&"))
    }
    def |(q: => Transformer[T]): Transformer[T] = {
      debugPrintln("| evaluated!")
      TransformerT(this orElse q, Some("|"))
    }
  }

  //Note that those accept by-name parameters!
  def Transformer[T](f: => TransformerBase) = TransformerT(f.asInstanceOf[PartialFunction[Exp[T], Exp[T]]])

  def TransformerT[T](f: => PartialFunction[Exp[T], Exp[T]], debugMsg: Option[String] = None) = {
    lazy val f0 = f //Without memoization, f would be recomputed over and over.
    //Having memoization here allows avoiding it in the combinators.
    new AbstractPartialFunction[Exp[T], Exp[T]] with Transformer[T] {
      override def applyOrElse[A1 <: Exp[T], B1 >: Exp[T]](x: A1, default: A1 => B1): B1 = {
        debugMsg match {
          case Some(debug) =>
            debugPrintln(s"Debug: result of $debug on $x")
          case _ =>
        }
        f0.applyOrElse(x, default)
      }
      override def isDefinedAt(x: Exp[T]) = f0.isDefinedAt(x)
    }
  }

  //This is the shortest way of writing identity.
  val emptyTransformOld: TransformerBase = {case e => e}

  def emptyTransform[T]: Transformer[T] = Transformer(emptyTransformOld)
  def kleeneStar[T](f: => Transformer[T]): Exp[T] => Exp[T] = {
    //Almost ideal definition: this works, and in fact it seems decently fast (in terms of reduction steps needed) thanks to the
    //laziness in the combinators
    //f & kleeneStar(f) | emptyTransform
    //But well, kleeneStar is where we do recursion, so tying the knot here is best. But we need lazy val, otherwise
    //this is pointless!
    lazy val resultFun: Exp[T] => Exp[T] = f & resultFun | emptyTransform
    resultFun
  }

  def fromPoly(e: Exp[Any] => Exp[Any]) = e

  def kleeneStarOld(f: TransformerBase): Exp[_] => Exp[_] = {
    //This is slower, because for each input it needs to reconstruct the function to apply.
    def resultFun(exp: Exp[_]): Exp[_] = (f andThen resultFun orElse emptyTransformOld)(exp)
    resultFun _
    //def resultFun: Exp[_] => Exp[_] = (f andThen resultFun orElse emptyTransform)
    //resultFun
  }
}

object TransformationCombinators extends TransformationCombinators[expressiontree.Exp]
