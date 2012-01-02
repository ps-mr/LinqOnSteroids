package ivm
package performancetests

import expressiontree.Exp
import optimization._
import Optimization._

object Benchmarking {
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

  def benchMarkTime(name: String, execLoops: Int = 1, warmUpLoops: Int = 10, sampleLoops: Int = 5, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
                   (toBench: => Unit) = {
    val (_, time) = benchMarkInternal(name, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)
    time
  }

  def benchMark[T](name: String, execLoops: Int = 1, warmUpLoops: Int = 10, sampleLoops: Int = 5, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): T = {
    val (ret, _) = benchMarkInternal(name, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)
    ret
  }
  /**
   * @param warmUpLoops: Warm up the VM - should be more
   * @param sampleLoops Iterations to measure variance.
   * @param toBench code to benchmark, which is supposed to always return the same value.
   * @returns the value returned by toBench
   */
  def benchMarkInternal[T](name: String, execLoops: Int = 1, warmUpLoops: Int = 10, sampleLoops: Int = 5, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): (T, Double) = {
    var ret: T = null.asInstanceOf[T]
    //Use Console.err instead of println and flush here.
    if (!hasConsoleOutput)
      println(">>> Name = %s, starting warmup..." format name)
    Console.flush()

    //We need at least one warmup loop to produce the result to return. That's not a perfect fix, but it doesn't matter.
    for (i <- 1 to math.max(warmUpLoops, 1))
      ret = toBench
    System.gc()

    if (hasConsoleOutput)
      println()
    else
      print(" ending warmup, starting benchmarking...")

    val stats = new VarianceCalc
    for (i <- 1 to sampleLoops) {
      val before = System.nanoTime()
      for (i <- 1 to execLoops)
        toBench
      stats.update((System.nanoTime() - before) / execLoops)
      System.gc()
    }
    if (!hasConsoleOutput)
      print(" ended benchmarking, name = %s, time = " format name)
    val avgMs = stats.avg / math.pow(10,6)
    if (verbose) {
      val devStdMs = math.sqrt(stats.variance) / math.pow(10,6)
      //The error of the measured average as an estimator of the average of the underlying random variable
      val stdErrMs = devStdMs / math.sqrt(sampleLoops)
      if (hasConsoleOutput)
        print(">>> Name = %s, time = " format name)
      println("(%.3f +- %.3f (stdErr = %.3f)) ms" format (avgMs, devStdMs, stdErrMs))
    }
    (ret, avgMs)
  }

  def silentBenchMark[T](name: String, execLoops: Int = 1, warmUpLoops: Int = 3, sampleLoops: Int = 3)
                     (toBench: => T): T =
    benchMark(name, execLoops, warmUpLoops, sampleLoops, false)(toBench)

  def printRes[T](v: Exp[T]) {
    println("v:\t\t" + v)
    println("optimize(v):\t" + optimize(v))
    println("v.interpret():\t" + v.interpret())
    //println("optimize(v).interpret():\t" + optimize(v).interpret())
    println()
  }

}
