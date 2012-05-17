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

trait OptParamSupport {
  //Does not work, report bug
  trait OptParam[+T]
  case object NoParam extends OptParam[Nothing]
  case class SomeParam[+T](t: T) extends OptParam[T]
  implicit def toSomeParam[T](t: T) = SomeParam(t)
}

trait QueryBenchmarking extends TestUtil with Benchmarking with OptParamSupport {
  this: ShouldMatchers =>

  def onlyOptimized = false
  //If we're running only the optimized version, we only run it only once.
  override def debugBench = super.debugBench || onlyOptimized

  private def doRun[T, Coll <: Traversable[T]](msg: String, v: Exp[Coll with Traversable[T]])(implicit f: Forceable[T, Coll]) = {
    if (!onlyOptimized)
      showExpNoVal(v, msg)
    benchMarkInternal(msg)(v.expResult().force)
  }

  private def benchOptimize[T, Coll <: Traversable[T]](msg: String, v: Exp[Coll with Traversable[T]]) = {
    if (!onlyOptimized)
      Optimization.optimize(v) //do it once for the logs!
    Optimization.pushEnableDebugLog(false)
    val (optimized, optimizationTime) = benchMarkInternal(msg + " Optimization")(Optimization.optimize(v))
    Optimization.popEnableDebugLog()
    (optimized, optimizationTime)
  }

  private def reportTimeRatio(label: String, speedup: Double) {
    val delta = 1 - speedup
    val lessOrMore = if (delta > 0) "less" else "MORE"
    println("Speedup ratio by this optimization compared to %s: %f (i.e. %f%% %s)" format (label, speedup, math.abs(delta) * 100, lessOrMore))
  }
  private def compare(time: Double, timeOpt: Double, optimizationTime: Double, timeScala: Double) = {
    reportTimeRatio("base embedded version", timeOpt / time)
    reportTimeRatio("native Scala version", timeOpt / timeScala)
    reportTimeRatio("native Scala version, counting optimization time", (timeOpt + optimizationTime) / timeScala)
  }

  private def benchInterpret[T, Coll <: Traversable[T]](msg: String,
                                                v: Exp[Coll with Traversable[T]],
                                                timeScala: Double)(implicit f: Forceable[T, Coll]): Traversable[T] =
  {
    val (optimized, optimizationTime) = benchOptimize(msg, v)
    val (resOpt, timeOpt) = doRun(msg + " - after optimization", optimized)
    if (!onlyOptimized) {
      val (res, time) = doRun(msg, v)
      //resOpt.toSet should be (res.toSet) //Broken, but what can we do? A query like
      // list.flatMap(listEl => set(listEl))
      //returns results in non-deterministic order.
      resOpt should be (res) //keep this and alter queries instead.

      compare(time, timeOpt, optimizationTime, timeScala)
    }
    resOpt
  }

  // The pattern matching costs of using force are quite annoying. I expect them to be small; so it would be be best to
  // just always have them, i.e. always check dynamically whether forcing is needed, for all LoS queries, slowing them
  // down a tiny insignificant bit.
  //XXX unused parameter: altExpected
  /**
   *
   * @param msg
   * @param expected
   * @param altExpected Optional by-name parameter, which shall never evaluate to null
   * @param query
   * @param altQueries
   * @tparam T
   * @tparam Coll
   * @return
   */
  def benchQueryComplete[T, Coll <: Traversable[T]](msg: String)
                                                   (expected: => Traversable[T], altExpected: => Traversable[T] = null /* Tried using OptParam here with */)
                                                   (query: => Exp[Coll], altQueries: Exp[Coll]*)
                                                   /*(implicit f: Forceable[T, Coll])*/ = {
    //Those versions don't work - bug https://issues.scala-lang.org/browse/SI-5642.
    //val builtQuery: Exp[Coll with Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = Query[T, Coll](toQuery(query))
    //works:
    val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query(query))

    //val res = benchInterpret("%s Los" format msg, builtQuery, timeScala)
    val losMsg = "%s Los" format msg
    val (optimized, optimizationTime) = benchOptimize(losMsg, builtQuery)
    val (resOpt, timeOpt) = doRun(losMsg + " - after optimization", optimized)

    if (!onlyOptimized) {
      val (expectedRes, timeScala) =
        benchMarkInternal(msg) { expected }
      val (resLos, timeLos) = doRun(losMsg, builtQuery)
      //resOpt.toSet should be (res.toSet) //Broken, but what can we do? A query like
      // list.flatMap(listEl => set(listEl))
      //returns results in non-deterministic order (which is arguably a Scala bug).
      //Alter queries instead: if they are non-deterministic they should return a set.

      compare(timeLos, timeOpt, optimizationTime, timeScala)

      resOpt should be (resLos)
      resLos should be (expectedRes)

      for ((altQuery, i) <- altQueries.zipWithIndex) {
        val altMsg = "%s Los - Alternative %d" format (msg, i)
        //Benchmark optimization time
        val (altOptimized, altOptimizationTime) = benchOptimize(altMsg, altQuery)
        //Check that the query produces the same result, and how much slower it is
        val (resAlt, timeAlt) = doRun(altMsg, altQuery: Exp[Traversable[T]])
        //Check that we get the same result
        resAlt should be (resOpt)
        reportTimeRatio("base embedded version - Alternative %d" format i, timeOpt / timeAlt)
        //Check that we get the same query
        altOptimized should be (optimized)

        //This code also measures performance of the optimized query, and does not check that the optimized query is the same as the other optimized query :-(.
        //benchInterpret[T, Traversable[T]]("%s Los - Alternative %d" format (msg, i), altQuery, timeScala)
      }
      val msgNativeAltExtra = "- Alternative (modularized)"
      val (altNativeRes, timeAltScala) =
        benchMarkInternal(msg + msgNativeAltExtra) { altExpected }
      if (altNativeRes != null) {
        altNativeRes should be (resOpt)
        reportTimeRatio("native Scala version - Alternative (modularized)", timeOpt / timeAltScala)
        reportTimeRatio("native Scala version - Alternative (modularized), counting optimization time", (timeOpt + optimizationTime) / timeAltScala)
      }
    }
    println("\tViolations: " + resOpt.size)

    resOpt

      /*
      showExpNoVal(altQuery
      altQuery.optimize
      val benchMarkInternal(msg2, altQuery, timeScala)
      should be (res)

    showExpNoVal(query, "modular version of query")
    val optQuery = query.optimize
    showExpNoVal(optQuery, "modular version of query - optimized")
    optQuery should be (queryBase.optimize)
    //val query2 =
    showExpNoVal(query2, "more modular version of query")
    val optQuery2 = query2.optimize
    showExpNoVal(optQuery2, "more modular version of query - optimized")
    optQuery2 should be (optQuery)*/
  }
}
