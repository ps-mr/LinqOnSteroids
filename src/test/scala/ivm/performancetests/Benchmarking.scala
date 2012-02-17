package ivm
package performancetests

import expressiontree.Exp
import optimization._
import Optimization._

object Benchmarking {
  val debugBench = false

  class VarianceCalc {
    var sum: Double = 0
    var sumSq: Double = 0
    var count = 0
    def update(sample: Double) {
      count += 1
      sum += sample
      sumSq += sample * sample
    }
    def avg = sum / count
    def variance = {
      val t = avg
      sumSq / count - t * t
    }
  }
}

trait Benchmarking {
  import Benchmarking._

  val execLoops = 1
  val warmUpLoops = 100
  val sampleLoops = 50

  //Import and re-export to inheritors.
  def debugBench = Benchmarking.debugBench

  //These are a def, so that overriding the values they depend on works!
  def effectiveExecLoops = if (debugBench) 1 else execLoops
  def effectiveWarmUpLoops = if (debugBench) 0 else warmUpLoops
  def effectiveSampleLoops = if (debugBench) 1 else sampleLoops

  def benchMarkTime(name: String, silent: Boolean = false, execLoops: Int = effectiveExecLoops, warmUpLoops: Int = effectiveWarmUpLoops, sampleLoops: Int = effectiveSampleLoops, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
                   (toBench: => Unit) = {
    val (_, time) = benchMarkInternal(name, silent, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)
    time
  }

  /**
   *
   * @param name
   * @param silent If silent is true, the benchmark output is supposed to still be saved (when output will be saved), but not displayed.
   * @param execLoops
   * @param warmUpLoops
   * @param sampleLoops
   * @param verbose
   * @param hasConsoleOutput
   * @param toBench
   * @tparam T
   * @return
   */
  def benchMark[T](name: String, silent: Boolean = false, execLoops: Int = effectiveExecLoops, warmUpLoops: Int = effectiveWarmUpLoops, sampleLoops: Int = effectiveSampleLoops, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): T = {
    val (ret, _) = benchMarkInternal(name, silent, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)
    ret
  }
  /**
   * @param warmUpLoops: Warm up the VM - should be more
   * @param sampleLoops Iterations to measure variance.
   * @param toBench code to benchmark, which is supposed to always return the same value.
   * @returns the value returned by toBench
   */
  private def benchMarkInternal[T](name: String, silent: Boolean, execLoops: Int, warmUpLoops: Int, sampleLoops: Int, verbose: Boolean, hasConsoleOutput: Boolean)
               (toBench: => T): (T, Double) = {
    var ret: T = null.asInstanceOf[T]
    if (!silent) {
      //XXX: Use Console.err instead of println and flush here.
      println("Benchmarking params: execLoops: %d, warmUpLoops: %d, sampleLoops: %d" format (execLoops, warmUpLoops, sampleLoops))
      if (!hasConsoleOutput)
        println(">>> Name = %s, starting warmup..." format name)
      Console.flush()
    }

    for (i <- 1 to warmUpLoops)
      toBench
    System.gc()

    if (!silent) {
      if (hasConsoleOutput)
        println()
      else
        print(" ending warmup, starting benchmarking...")
    }

    val stats = new VarianceCalc
    for (i <- 1 to sampleLoops) {
      val before = System.nanoTime()
      for (i <- 1 to execLoops)
        ret = toBench
      stats.update((System.nanoTime() - before) / execLoops)
      System.gc()
    }
    if (!hasConsoleOutput && !silent)
      print(" ended benchmarking, name = %s, time = " format name)
    val avgMs = stats.avg / math.pow(10,6)
    if (verbose) {
      val devStdMs = math.sqrt(stats.variance) / math.pow(10,6)
      //The error of the measured average as an estimator of the average of the underlying random variable
      val stdErrMs = devStdMs / math.sqrt(sampleLoops)
      if (!silent) {
        if (hasConsoleOutput)
          print(">>> Name = %s, time = " format name)
        println("(%.3f +- %.3f (stdErr = %.3f)) ms" format (avgMs, devStdMs, stdErrMs))
      }
    }
    (ret, avgMs)
  }

  /*
  def silentBenchMark[T](name: String, execLoops: Int = 1, warmUpLoops: Int = 3, sampleLoops: Int = 3)
                     (toBench: => T): T =
    benchMark(name, execLoops, warmUpLoops, sampleLoops, false)(toBench)
    */

  def printRes[T](v: Exp[T]) {
    println("v:\t\t" + v)
    println("optimize(v):\t" + optimize(v))
    println("v.interpret():\t" + v.interpret())
    //println("optimize(v).interpret():\t" + optimize(v).interpret())
    println()
  }

}
