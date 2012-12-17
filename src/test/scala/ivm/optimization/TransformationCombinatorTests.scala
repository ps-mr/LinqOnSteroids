package ivm.optimization

import ivm.expressiontree._
import Lifting._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * User: pgiarrusso
 * Date: 17/12/2012
 */
class TransformationCombinatorTests extends TransformationCombinators[Exp] with FunSuite with ShouldMatchers {
  import OptimizationTransforms.{deltaReductionTuple, betaReduction}

  //def betaDeltaReducer2 = kleeneStar(deltaReductionTuple orElse betaReduction)
  val betaDeltaReducer2 = fromPoly(kleeneStar(Transformer { deltaReductionTuple orElse betaReduction }))

  def applyFun[A, B] = {
    //\x f -> f x
    FunSym(Fun((x: Exp[A]) => Fun((f: Exp[A => B]) => f(x))))
  }
  def applyFun2[A, B]: Exp[(A => B) => (A => B)] = {
    //\f x -> f x
    FunSym(Fun((f: Exp[A => B]) => FunSym(Fun(f(_)))))
  }
  def applyIdFun[A, B]: Exp[A => A] = {
    //\x -> applyFun x id
    FunSym(Fun((x: Exp[A]) => applyFun(x)(FunSym(Fun(x => x)))))
  }
  def applyIdFunMoreComplex[A, B]: Exp[A => A] = {
    //\x -> applyFun x id
    //Fun((x: Exp[A]) => applyFun(x)(Fun(x => x)))
    FunSym(Fun((x: Exp[A]) => (letExp(x)(applyFun[A, A].f))(FunSym(Fun(x => x)))))
  }

  test("combinators") {
    for (term <- Seq(applyIdFun, applyIdFunMoreComplex)) {
      println("Term:" + term)
      val opt1 = term.transform(betaDeltaReducer2)
      println("Beta-reduced term:" + opt1)
      assert(opt1 == FunSym(Fun[Int, Int](x => x)))
      val opt2 = term.transform(TransformationCombinatorsExperiments.TransformationCombinators.betaDeltaReducer3)
      println("Beta-reduced term - with Scalaz:" + opt2)
      assert(opt2 == opt1)
      println()
    }
  }
}
