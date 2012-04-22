package ivm
package optimization

import expressiontree.{Lifting, App, Fun, ExpProduct, ExpSelection, Exp}
import Lifting._

//import scalaz._


/**
 * User: pgiarrusso
 * Date: 20/4/2012
 */

class TransformationCombinators {
  //implicit val m: Monoid[PartialFunction[Exp[_], Exp[_]]]
  type Transformer = PartialFunction[Exp[_], Exp[_]]
  //This is the shortest way of writing identity.
  val emptyTransform: Transformer = {case e => e}

  def kleeneStar(f: Transformer): Exp[_] => Exp[_] = {
    //def resultFun(exp: Exp[_]): Exp[_] = (f andThen resultFun orElse emptyTransform)(exp)
    //resultFun _
    def resultFun: Exp[_] => Exp[_] = (f andThen resultFun orElse emptyTransform)
    resultFun
  }
  /*
  //Ideal definition, maybe - this should at least work, if not very efficiently because of the lack of sharing:
  def kleeneStar(f: => Transformer): Exp[_] => Exp[_] =
    f andThen kleeneStar(f) orElse emptyTransform
  */
}

object TransformationCombinators extends TransformationCombinators /*with App*/ {
  import OptimizationTransforms.{deltaReductionTuple,betaReduction}

  def betaDeltaReducer2 = kleeneStar(deltaReductionTuple orElse betaReduction)
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
      println(betaDeltaReducer2(term))
      println("Beta-reduced term:" + term.transform(betaDeltaReducer2))
      println()
    }
  }
}
