package ivm
package optimization

import expressiontree.{Lifting, Fun, Exp}
import Lifting._

import scalaz._
import Scalaz._

/**
 * User: pgiarrusso
 * Date: 20/4/2012
 */

//TODO: For M[_] = Option, add overloads for PartialFunctions.
class TransformationCombinatorsScalaz[T] {
  type TransformerFun[M[_]] = T => M[T]
  // This implicit conversion makes crazy Scalaz methods (like >=>) available on TransformerFun.
  implicit def toKeisli[M[_]: Monad](f: TransformerFun[M]): Kleisli[M, T, T] = kleisli(f)

  abstract class Transformer[M[_]] extends TransformerFun[M] {
    //XXX: this definition of map is very different from Parser combinator's map or ^^ operator.
    def map(q: => T => T)(implicit m: Functor[M]): Transformer[M] = {
      lazy val q0 = q
      Transformer { kleisli(this) map q0 }
    }

    //To implement ^^, we need to change the used monad.
    //XXX: However, the used monad M is a free variable in Transformer, not a parameter. Hence we cannot build a
    // Transformer here
    def compose[N[_]](f: M[T] => N[T]): Kleisli[N, T, T] = this compose f
    def &(q: => Transformer[M])(implicit m: Monad[M]): Transformer[M] = {
      lazy val q0 = q
      Transformer { this >=> q0 }
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

  //implicitly[C]
  //implicitly[Endo[Exp[_]]]
  //implicit val m: Monoid[PartialFunction[Exp[_], Exp[_]]]
  type Transformer = PartialFunction[Exp[_], Exp[_]]
  abstract class Transformer2 extends PartialFunction[Exp[_], Exp[_]] {
    def &(q: => (Exp[_] => Exp[_])) = {
      //Transformer2 {}
      lazy val q0 = q
      //this andThen q0 //Eta-expansion should be applied _here_
      Transformer2 { case in if isDefinedAt(in) => q0(this(in)) }
    }
    /*def |(q: => Transformer2) = {
      //Transformer2 {}
      lazy val q0 = q
      this orElse q0 //Eta-expansion should be applied _here_
    }*/
  }
  def Transformer2(f: Transformer) = new Transformer2 {
    def apply(in: Exp[_]) = f(in)
    def isDefinedAt(x: Exp[_]) = f.isDefinedAt(x)
  }

  //This is the shortest way of writing identity.
  val emptyTransform: Transformer = {case e => e}

  val emptyTransform2: Transformer2 = Transformer2(emptyTransform)
  def kleeneStar2(f: => Transformer2): Exp[_] => Exp[_] =
    f & kleeneStar2(f) orElse emptyTransform

  def kleeneStar(f: Transformer): Exp[_] => Exp[_] = {
    def resultFun(exp: Exp[_]): Exp[_] = (f andThen resultFun orElse emptyTransform)(exp)
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

object TransformationCombinators extends TransformationCombinators /*with App*/ {
  import OptimizationTransforms.{deltaReductionTuple, betaReduction}

  //def betaDeltaReducer2 = kleeneStar(deltaReductionTuple orElse betaReduction)
  def betaDeltaReducer2 = kleeneStar2(Transformer2 { deltaReductionTuple orElse betaReduction })

  object Foo extends TransformationCombinatorsScalaz[Exp[_]]
  def betaDeltaReducer3 = {
    import Foo._
    Function.unlift(Transformer {(deltaReductionTuple orElse betaReduction).lift} *)
  }

  def applyFun[A, B] = {
    //\x f -> f x
    Fun((x: Exp[A]) => Fun((f: Exp[A => B]) => f(x)))
  }
  def applyFun2[A, B]: Exp[(A => B) => (A => B)] = {
    //\f x -> f x
    Fun((f: Exp[A => B]) => Fun(f(_)))
  }
  def applyIdFun[A, B]: Exp[A => A] = {
    //\x -> applyFun x id
    Fun((x: Exp[A]) => applyFun(x)(Fun(x => x)))
  }
  def applyIdFunMoreComplex[A, B]: Exp[A => A] = {
    //\x -> applyFun x id
    //Fun((x: Exp[A]) => applyFun(x)(Fun(x => x)))
    Fun((x: Exp[A]) => (letExp(x)(applyFun[A, A].f))(Fun(x => x)))
  }

  def main(args: Array[String]) {
    for (term <- Seq(applyIdFun, applyIdFunMoreComplex)) {
      println("Term:" + term)
      val opt1 = term.transform(betaDeltaReducer2)
      println("Beta-reduced term:" + opt1)
      assert(opt1 == Fun[Int, Int](x => x))
      val opt2 = term.transform(betaDeltaReducer3)
      println("Beta-reduced term - with Scalaz:" + opt2)
      assert(opt2 == opt1)
      println()
    }
  }
}
