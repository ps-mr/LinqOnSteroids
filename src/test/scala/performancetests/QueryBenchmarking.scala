package performancetests

import ivm._
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
      benchMarkInternal(msg)(v.expResult().force)
    }

    val (res, time) = doRun(msg, v)
    for ((msgExtra, optim) <- optimizerTable[Coll] ++ extraOptims.asInstanceOf[Seq[(String, Exp[Coll] => Exp[Coll])]]) {
      val (resOpt, timeOpt) = doRun(msg + msgExtra, optim(Optimization.optimize(v)))
      resOpt should be (res)
      println("Speedup by this optimization: %f" format (timeOpt.asInstanceOf[Float] / time))
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

  // The pattern matching costs of using force are quite annoying. I expect them to be small; so it would be be best to
  // just always have them, i.e. always check dynamically whether forcing is needed, for all LoS queries, slowing them
  // down a tiny insignificant bit.
  def benchQueryComplete[T, Coll <: Traversable[T]](msg: String)
                                                   (expected: => Traversable[T], doBench: Boolean = true)
                                                   (query: => Exp[Coll],
                                                    extraOptims: Seq[(String, Exp[Nothing] => Exp[Nothing])] = Seq.empty)
                                                   /*(implicit f: Forceable[T, Coll])*/ = {
    val (expectedRes, time) =
      if (doBench)
        benchMarkInternal(msg)(expected)
      else
        (expected, 1)
    val builtQuery = benchMark("%s Los Setup" format msg, silent = true)(Query(query))
    benchQuery("%s Los" format msg, builtQuery, expectedRes, extraOptims)
    if (doBench)
      println("\tViolations: " + expectedRes.size)
    expectedRes
  }
}
