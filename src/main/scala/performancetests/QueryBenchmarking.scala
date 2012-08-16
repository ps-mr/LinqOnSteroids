package performancetests

import ivm._
import tests.TestUtil
import expressiontree.{Compile, Lifting, Exp, TypeTag}
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

  private def mustModularizedOptimizeEqual = false

  private def printDate() {
    import java.text.DateFormat
    import java.util.Calendar
    import java.util.Date

//    val cal = Calendar.getInstance
//    val df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM)
//
//    println(df.format(cal.getTime))
    println(new Date)
  }

  private def doRun[T, Coll <: Traversable[T]](msg: String, v: Exp[Coll with Traversable[T]])(implicit f: Forceable[T, Coll], c: TypeTag[Coll]) = {
    printDate()
    if (!onlyOptimized)
      showExpNoVal(v, msg)
    val res = benchMarkWithTime(msg)(Compile.toValue(v).force)
    //if (!onlyOptimized)
      //res._1 should be (v.value().force)
    res
  }

  private def benchOptimize[T, Coll <: Traversable[T]](msg: String, v: Exp[Coll with Traversable[T]], idxLookup: Boolean) = {
    if (!onlyOptimized)
      Optimization.optimize(v) //do it once for the logs!
    Optimization.pushEnableDebugLog(false)
    val (optimized, optimizationTime) = benchMarkWithTime(msg + " Optimization")(Optimization.optimize(v, idxLookup))
    Optimization.popEnableDebugLog()
    (optimized, optimizationTime)
  }

  private def reportTimeRatio(label: String, speedup: Double) {
    val delta = 1 - speedup
    val lessOrMore = if (delta > 0) "less" else "MORE"
    //All callers supply the slowdown ratio, so invert it to get the speedup ratio.
    println("Speedup ratio by this optimization compared to %s: %f (i.e. %f%% %s)" format (label, 1 / speedup, math.abs(delta) * 100, lessOrMore))
  }
  private def compare(time: Double, timeOpt: Double, optimizationTime: Double, timeScala: Double) = {
    reportTimeRatio("base embedded version", timeOpt / time)
    reportTimeRatio("native Scala version", timeOpt / timeScala)
    reportTimeRatio("native Scala version, counting optimization time", (timeOpt + optimizationTime) / timeScala)
  }

  // The pattern matching costs of using force are quite annoying. I expect them to be small; so it would be be best to
  // just always have them, i.e. always check dynamically whether forcing is needed, for all LoS queries, slowing them
  // down a tiny insignificant bit.
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
   //Since 2.10.0-M6, Coll is deduced to be Traversable[Nothing].
  //def benchQueryComplete[T, Coll <: Traversable[T]](msg: String)
                                                   //(expected: => Traversable[T], altExpected: => Traversable[T] = null /* Tried using OptParam here with */)
                                                   //(query: => Exp[Coll], altQueries: Exp[Coll]*)(implicit cm: TypeTag[Traversable[T]])
  def benchQueryComplete[T](msg: String)
                                                   (expected: => Traversable[T], altExpected: => Traversable[T] = null /* Tried using OptParam here with */)
                                                   (query: => Exp[Traversable[T]], altQueries: Exp[Traversable[T]]*)(implicit cm: TypeTag[Traversable[T]])
                                                   /*(implicit f: Forceable[T, Coll])*/ = {
    //Those versions don't work - bug https://issues.scala-lang.org/browse/SI-5642.
    //val builtQuery: Exp[Coll with Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query[T, Coll](query))
    //val builtQuery: Exp[Traversable[T]] = Query[T, Coll](toQuery(query))
    //works:
    val builtQuery: Exp[Traversable[T]] = benchMark("%s Los Setup" format msg, silent = true)(Query(query))

    //val res = benchInterpret("%s Los" format msg, builtQuery, timeScala)
    val losMsg = "%s Los" format msg
    val (optimized, optimizationTime) = benchOptimize(losMsg, builtQuery, idxLookup = true)
    //we should do this for the modularized version.
    //val (optimizedWithIdx, optimizationTimeWithIdx) = benchOptimize(losMsg, builtQuery, idxLookup = true)
    val optimizedWithIdxMsg = " - after optimization"
    val optimizedMsg = optimizedWithIdxMsg + " (without indexes)"
    val (resOpt, timeOpt) = doRun(losMsg + optimizedWithIdxMsg, optimized)

    /*
    if (optimizedWithIdx != optimized) {
      val (resOptWithIdx, timeOptWithIdx) = doRun(losMsg + optimizedWithIdxMsg, optimizedWithIdx)
      resOptWithIdx should be (resOpt)
    }
    */

    if (!onlyOptimized) {
      val (expectedRes, timeScala) =
        benchMarkWithTime(msg) { expected }
      val (resLos, timeLos) = doRun(losMsg, builtQuery)
      //resOpt.toSet should be (res.toSet) //Broken, but what can we do? A query like
      // list.flatMap(listEl => set(listEl))
      //returns results in non-deterministic order (which is arguably a Scala bug).
      //Alter queries instead: if they are non-deterministic they should return a set.
      //I changed my mind: we will always compare results after turning them into Sets.
      //That's a good way to pretend we have Bag semantics.

      compare(timeLos, timeOpt, optimizationTime, timeScala)

      resOpt.toSet should be (resLos.toSet)
      resLos.toSet should be (expectedRes.toSet)

      val optimizedQueries = for {
        (altQuery, i) <- altQueries.zipWithIndex
        altMsg = "%s Los - Alternative %d" format (msg, i)
        //Benchmark optimization time
        (altOptimized, _ /*altOptimizationTime*/) = benchOptimize(altMsg, altQuery, idxLookup = false)
        (altOptimizedWithIdx, _ /*altOptimizationTime*/) = benchOptimize(altMsg, altQuery, idxLookup = true)
      } yield (altMsg, altOptimized, altOptimizedWithIdx, altQuery, i)

      for ((altMsg, altOptimized, altOptimizedWithIdx, altQuery, i) <- optimizedQueries) {
        //Check that the query produces the same result *before* optimization, and how much slower it is
        val (resAlt, timeAlt) = doRun(altMsg, altQuery: Exp[Traversable[T]])
        //Check that we get the same result
        resAlt.toSet should be (resOpt.toSet)
        //resAlt should be (resOpt)
        reportTimeRatio("base embedded version - Alternative %d (non optimized)" format i, timeOpt / timeAlt)
        if (altOptimizedWithIdx != optimized) {
          val (resAltOptWithIdx, timeAltOptWithIdx) = doRun(altMsg + optimizedWithIdxMsg, altOptimizedWithIdx)
          resAltOptWithIdx should be (resOpt)
        }
      }
      val msgNativeAltExtra = " - Alternative (modularized)"
      val (altNativeRes, timeAltScala) =
        benchMarkWithTime(msg + msgNativeAltExtra) { altExpected }
      if (altNativeRes != null) {
        altNativeRes.toSet should be (resOpt.toSet)
        reportTimeRatio("native Scala version%s" format msgNativeAltExtra, timeOpt / timeAltScala)
        reportTimeRatio("native Scala version%s, counting optimization time" format msgNativeAltExtra, (timeOpt + optimizationTime) / timeAltScala)
      }

      for ((altMsg, altOptimized, _, _, i) <- optimizedQueries) {
        //Check that we get the same query by optimizing modularized queries and non-modularized ones - I expect failures here.
        if (mustModularizedOptimizeEqual) {
          altOptimized should be (optimized)
        } else if (altOptimized != optimized) {
          import compat.Platform.EOL
          val indent = "    "
          //Console.err.printf("### %s, after optimization, is != optimized\naltOptimized = %s\noptimized = %s\nAt:\n%s\n",
           // altMsg, altOptimized, optimized, new Throwable().getStackTrace mkString (indent, EOL + indent, EOL))
          printf("### %s, after optimization, is != optimized\naltOptimized = %s\noptimized = %s\n",
            altMsg, altOptimized, optimized)
          //Since the result of optimization of the modularized query is in fact different (presumably worse?),
          //let's compare its performance to the optimized non-modular query.
          val (resAltOpt, timeAltOpt) = doRun(altMsg + optimizedMsg, altOptimized: Exp[Traversable[T]])
          resAltOpt.toSet should be (resOpt.toSet)
          //resAltOpt should be (resOpt)
          reportTimeRatio("base embedded version - Alternative %d (optimized)" format i, timeOpt / timeAltOpt)
        }
      }
    }
    println("\tViolations: " + resOpt.size)

    resOpt
  }
}
