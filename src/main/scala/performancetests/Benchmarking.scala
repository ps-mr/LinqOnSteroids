package performancetests

import ivm.GitVersion
import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.{PrintWriter, FileWriter, BufferedWriter}

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
  private val testDate = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(Calendar.getInstance.getTime)
  private val logPath = "LOSTestLog.csv"
  private val logWriter = new PrintWriter(new BufferedWriter(new FileWriter(logPath, true)))
  private var usedNames = Set[String]()
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
                   (toBench: => Unit) =
    benchMarkInternal(name, silent, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)._2

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
               (toBench: => T): T =
    benchMarkInternal(name, silent, execLoops, warmUpLoops, sampleLoops, verbose, hasConsoleOutput)(toBench)._1
  /**
   * @param warmUpLoops: Warm up the VM - should be more
   * @param sampleLoops Iterations to measure variance.
   * @param toBench code to benchmark, which is supposed to always return the same value.
   * @return the value returned by toBench
   */
  def benchMarkInternal[T](name: String, silent: Boolean = false, execLoops: Int = effectiveExecLoops, warmUpLoops: Int = effectiveWarmUpLoops, sampleLoops: Int = effectiveSampleLoops, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): (T, Double) = {
    def print(x: Any) = if (!silent) Console.err.print(x)
    def println(x: Any) = if (!silent) Console.err.println(x)
    //Why not call this println()? Because overloading is not supported in local definitions (SLS §6.11).
    def newLine() = if (!silent) Console.err.println()

    if (usedNames(name))
      println("WARNING: benchmark name %s already used" format name)
    else
      usedNames += name

    var ret: T = null.asInstanceOf[T]
    newLine() //Make space at the beginning
    println("Benchmarking params: execLoops: %d, warmUpLoops: %d, sampleLoops: %d" format (execLoops, warmUpLoops, sampleLoops))
    if (!hasConsoleOutput)
      println(">>> Name = %s, starting warmup..." format name)
    //Use Console.err instead of println and flush here.
    //Console.flush()

    for (i <- 1 to warmUpLoops)
      toBench
    if (!debugBench)
      System.gc()

    if (hasConsoleOutput)
      newLine()
    else
      print(" ending warmup, starting benchmarking...")

    val stats = new VarianceCalc
    for (i <- 1 to sampleLoops) {
      val before = System.nanoTime()
      for (j <- 1 to execLoops)
        ret = toBench
      stats.update((System.nanoTime() - before) / execLoops)
      if (!debugBench)
        System.gc()
    }
    if (!hasConsoleOutput)
      print(" ended benchmarking, name = %s, time = " format name)
    val avgMs = stats.avg / math.pow(10,6)
    val devStdMs = math.sqrt(stats.variance) / math.pow(10,6)
    //The error of the measured average as an estimator of the average of the underlying random variable
    val stdErrMs = devStdMs / math.sqrt(sampleLoops)

    if (verbose) {
      if (hasConsoleOutput)
        print(">>> Name = %s, time = " format name)
      println("(%.3f +- %.3f (stdErr = %.3f)) ms" format (avgMs, devStdMs, stdErrMs))
    }
    //Format output for Jenkins' Measurement Plot plugin - https://wiki.jenkins-ci.org/display/JENKINS/Measurement+Plots+Plugin
    println(<measurement><name>{name}</name><value>{avgMs}</value></measurement>)
    //Format output for R
    logWriter.println("%s;%s;%s;%f;%f;%f" format (GitVersion.version, testDate, name.replace(';', '_'), avgMs, devStdMs, stdErrMs))
    logWriter.flush()

    (ret, avgMs)
  }

  /*
  def silentBenchMark[T](name: String, execLoops: Int = 1, warmUpLoops: Int = 3, sampleLoops: Int = 3)
                     (toBench: => T): T =
    benchMark(name, execLoops, warmUpLoops, sampleLoops, false)(toBench)
    */

  /*
  def printRes[T](v: Exp[T]) {
    println("v:\t\t" + v)
    println("optimize(v):\t" + optimize(v))
    println("v.interpret():\t" + v.interpret())
    //println("optimize(v).interpret():\t" + optimize(v).interpret())
    println()
  }
  */
}
