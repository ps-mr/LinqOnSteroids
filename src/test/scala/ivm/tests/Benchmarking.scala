package ivm
package tests

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

  def benchMark(name: String)(toBench: => Unit) {
    //Iteration counts.
    //Warm up the VM - should be more
    val warmUpLoops = 10

    //Repetitions to increase precisions.
    val execLoops = 1
    //Iterations to measure variance.
    val sampleLoops = 5

    for (i <- 1 to warmUpLoops)
      toBench
    println()

    val stats = new VarianceCalc
    for (i <- 1 to sampleLoops) {
      val before = System.nanoTime()
      for (i <- 1 to execLoops)
        toBench
      println()
      stats.update((System.nanoTime() - before) / execLoops)
    }
    println(">>> Name = %s, time = %.3f +- %.3f" format (name,
      stats.avg / math.pow(10, 6),
      math.sqrt(stats.variance) / math.pow(10, 6)))
  }

  def printRes[T](v: Exp[T]) {
    println("v:\t\t" + v)
    println("optimize(v):\t" + optimize(v))
    println("v.interpret:\t" + v.interpret)
    //println("optimize(v).interpret:\t" + optimize(v).interpret)
    println()
  }

}
