package ivm
package opaltests

import performancetests.Benchmarking
import tests.TestUtil
import expressiontree.{Lifting, Exp}
import Lifting._
import optimization.Optimization
import org.scalatest.matchers.ShouldMatchers

/**
 * User: pgiarrusso
 * Date: 27/1/2012
 */

trait QueryBenchmarking extends TestUtil with Benchmarking {
  this: ShouldMatchers =>
  def optimizerTable[T]: Seq[(String, Exp[T] => Exp[T])] = Seq((" - after optimization", identity _))

  def benchInterpret[T, Coll <: Traversable[T]](msg: String,
                                                v: Exp[Coll],
                                                extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty)(implicit f: Forceable[T, Coll]): Traversable[T] =
  {
    def doRun(msg: String, v: Exp[Coll]) = {
      showExpNoVal(v, msg)
      benchMark(msg)(v.expResult().force)
    }

    val res = doRun(msg, v)
    for ((msgExtra, optim) <- optimizerTable[Coll] ++ extraOptims.asInstanceOf[Seq[(String, Exp[Coll] => Exp[Coll])]]) {
      val resOpt = doRun(msg + msgExtra, optim(Optimization.optimize(v)))
      resOpt should be (res)
    }

    res
  }

  def benchQuery[T, Coll <: Traversable[T]](msg: String,
                                            v: Exp[Coll],
                                            expectedResult: Traversable[T],
                                            extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty)(implicit f: Forceable[T, Coll]): Traversable[T] = {
    val res = benchInterpret[T, Coll](msg, v, extraOptims)
    res should be (expectedResult)
    res
  }
}
