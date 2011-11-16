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

  /**
   * @param warmuUpLoops: Warm up the VM - should be more
   * @param sampleLoops Iterations to measure variance.
   */
  def benchMark(name: String, execLoops: Int = 1, warmUpLoops: Int = 10, sampleLoops: Int = 5, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => Unit): Double = {
    if (!hasConsoleOutput)
      print(">>> Name = %s, starting warmup..." format name)

    for (i <- 1 to warmUpLoops)
      toBench

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
    avgMs
  }

  def silentBenchMark(name: String, execLoops: Int = 1, warmUpLoops: Int = 3, sampleLoops: Int = 3)
                     (toBench: => Unit) =
    benchMark(name, execLoops, warmUpLoops, sampleLoops, false)(toBench)

  def printRes[T](v: Exp[T]) {
    println("v:\t\t" + v)
    println("optimize(v):\t" + optimize(v))
    println("v.interpret:\t" + v.interpret)
    //println("optimize(v).interpret:\t" + optimize(v).interpret)
    println()
  }

}
