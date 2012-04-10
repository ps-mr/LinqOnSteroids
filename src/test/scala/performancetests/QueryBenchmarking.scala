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

  def onlyOptimized = false

  private def benchInterpret[T, Coll <: Traversable[T]](msg: String,
                                                v: Exp[Coll with Traversable[T]],
                                                timeScala: Double)(implicit f: Forceable[T, Coll]): Traversable[T] =
  {
    def doRun(msg: String, v: Exp[Coll]) = {
      if (!onlyOptimized)
        showExpNoVal(v, msg)
      benchMarkInternal(msg)(v.expResult().force)
    }

    val (res, time) =
      if (!onlyOptimized)
        doRun(msg, v)
      else
        (null, -1.0)
    val msgExtra = " - after optimization";
    {
      if (!onlyOptimized)
        Optimization.optimize(v) //do it once for the logs!
      Optimization.pushEnableDebugLog(false)
      val (optimized, optimizationTime) = benchMarkInternal(msg + " Optimization")(Optimization.optimize(v))
      Optimization.popEnableDebugLog()
      val (resOpt, timeOpt) = doRun(msg + msgExtra, optimized)
      if (!onlyOptimized) {
        //resOpt.toSet should be (res.toSet) //Broken, but what can we do? A query like
        // list.flatMap(listEl => set(listEl))
        //returns results in non-deterministic order.
        resOpt should be (res) //keep this and alter queries instead.

        def report(label: String, speedup: Double) {
          val delta = 1 - speedup
          val lessOrMore = if (delta > 0) "less" else "MORE"
          println("Speedup ratio by this optimization compared to %s: %f (i.e. %f%% %s)" format (label, speedup, math.abs(delta) * 100, lessOrMore))
        }
        report("base embedded version", timeOpt / time)
        report("native Scala version", timeOpt / timeScala)
        report("native Scala version, counting optimization time", (timeOpt + optimizationTime) / timeScala)
      }
      resOpt
    }
  }

  // The pattern matching costs of using force are quite annoying. I expect them to be small; so it would be be best to
  // just always have them, i.e. always check dynamically whether forcing is needed, for all LoS queries, slowing them
  // down a tiny insignificant bit.
  def benchQueryComplete[T, Coll <: Traversable[T]](msg: String)
                                                   (expected: => Traversable[T])
                                                   (query: => Exp[Coll])
                                                   /*(implicit f: Forceable[T, Coll])*/ = {
    val (expectedRes, timeScala) =
      if (!onlyOptimized)
        benchMarkInternal(msg)(expected)
      else
        (null, -1.0)
    //Those versions don't work - bug https://issues.scala-lang.org/browse/SI-5642.
    //val builtQuery: Exp[Coll with Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = Query[T, Coll](toQuery(query))
    //works:
    val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query(query))
    val res = benchInterpret("%s Los" format msg, builtQuery, timeScala)
    if (!onlyOptimized) {
      res should be (expectedRes)
    }
    println("\tViolations: " + res.size)
    res
  }
}
