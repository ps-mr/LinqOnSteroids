package performancetests

import ivm.GitVersion
import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.{PrintWriter, FileWriter, BufferedWriter}
import collection.mutable.ArrayBuffer
import java.{util => jUtil, text => jText}

/**
 * The goal of this trait is to implement steady-state performance evaluation as detailed by Georges et al. [1, Sec. 4.2],
 * through its benchMark* methods, and save the data in a form which can be easily post-processed.
 * A benchMark invocation performs only the benchmark iterations within a single JVM invocation. To perform multiple JVM
 * invocations, an external harness needs to be setup -- the one we use is not really intended to be reusable.
 *
 * [1] Georges, A., Buytaert, D., and Eeckhout, L. 2007. "Statistically rigorous Java performance evaluation."
 *     In Proc. Int’l Conf. Object-Oriented Programming, Systems, Languages and Applications,
 *     OOPSLA ’07, ACM, pp. 57–76.
 */
trait Benchmarking {
  import Benchmarking._

  //Defaults for benchmarking params which can be overriden by subclasses
  protected val defaultExecLoops = 1
  protected val defaultMinSampleLoops = 10
  /** Maximum Coefficient of Variation between iterations. */
  protected val defaultMaxCoV = Some(0.02)

  //Other settings, not intended to be customizable.
  /** How many benchmark iterations should be remembered? Called k in the paper. */
  protected val rememberedSampleLoops = 50

  /** How many benchmark iterations at most should be performed? Called q in the paper. */
  protected val maxLoops = 1000

  protected val printAllData = false

  /** Invoke the garbage collector between benchmark iterations. This adds a tremendous slowdown for small benchmarks. */
  protected val callGC = false

  protected val minIterationIntervalNanoSec = 10 * math.pow(10, 9)

  //Import and re-export to inheritors.
  def debugBench = Benchmarking.debugBench

  def benchMarkTime(name: String, silent: Boolean = false, execLoops: Int = defaultExecLoops, minSampleLoops: Int = defaultMinSampleLoops, maxCoV: Option[Double] = defaultMaxCoV, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
                   (toBench: => Unit) =
    benchMarkInternal(name, silent, execLoops, minSampleLoops, maxCoV, verbose, hasConsoleOutput)(toBench)._2

  /**
   *
   * @param name name for the benchmark to use in output
   * @param silent If silent is true, the benchmark output is supposed to still be saved (when output will be saved), but not displayed.
   * @param execLoops Number of times to execute the benchmark in each inner loop; time is taken every execLoops iterations.
   * @param minSampleLoops Minimum number of benchmark iterations.
   * @param verbose
   * @param hasConsoleOutput Does the benchmarked code produce console output?
   * @param toBench Code to benchmark
   * @param maxCoV If Some(x), x is the maximum coefficient of variation allowed.
   * @tparam T
   * @return The return value of executing toBench, assuming it always returns the same value.
   */
  def benchMark[T](name: String, silent: Boolean = false, execLoops: Int = defaultExecLoops, minSampleLoops: Int = defaultMinSampleLoops, maxCoV: Option[Double] = defaultMaxCoV, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): T =
    benchMarkInternal(name, silent, execLoops, minSampleLoops, maxCoV, verbose, hasConsoleOutput)(toBench)._1

  /**
   * @param minSampleLoops Iterations to measure variance.
   * @param toBench code to benchmark, which is supposed to always return the same value.
   * @return the value returned by toBench
   */
  def benchMarkInternal[T](name: String, silent: Boolean = false, execLoops: Int = defaultExecLoops, minSampleLoops: Int = defaultMinSampleLoops, maxCoV: Option[Double] = defaultMaxCoV, verbose: Boolean = true, hasConsoleOutput: Boolean = false)
               (toBench: => T): (T, Double) = {
    def print(x: Any) = if (!silent) Console.err.print(x)
    def println(x: Any) = if (!silent) Console.err.println(x)
    //Why not call this println()? Because overloading is not supported in local definitions (SLS §6.11).
    def newLine() = if (!silent) Console.err.println()
    def gcAndSnapshotUsedMemory() =
      if (!debugBench)
        MemoryUsage.gcAndSnapshotUsedMemory() //This call is expensive because it invokes GC - on my machine it takes 240 ms.
      else
        0L


    val benchmarkingBegin = System.nanoTime()
    if (usedNames(name))
      println("WARNING: benchmark name %s already used" format name)
    else
      usedNames += name

    var ret: T = null.asInstanceOf[T]
    newLine() //Make space at the beginning
    println("Benchmarking params: execLoops: %d, minSampleLoops: %d, maxCoV: %s" format (execLoops, minSampleLoops, maxCoV))
    if (!hasConsoleOutput) {
      print(">>> Name = %s" format name)
      newLine()
    }
    //Use Console.err instead of println and flush here.
    //Console.flush()

    val memoryBefore = gcAndSnapshotUsedMemory()

    if (hasConsoleOutput)
      newLine()
    else
      print(" starting benchmarking...")

    val stats = new VarianceCalc(rememberedSampleLoops)
    val values = ArrayBuffer[Long]()
    var lastPrintTime = System.nanoTime()

    var i = 0
    do {
      val before = System.nanoTime()
      for (j <- 1 to execLoops)
        ret = toBench
      val after = System.nanoTime()
      val timeD = (after - before) / execLoops
      //print("%d;" format timeD)
      stats.update(timeD)
      values += timeD
      if (callGC && !debugBench)
        System.gc()
      i += 1
      if (after - lastPrintTime > minIterationIntervalNanoSec) {
        println("Iteration %d, used memory %,d".format(i, gcAndSnapshotUsedMemory() - memoryBefore))
        lastPrintTime = after
      }
      //If debugBench, we never want to reiterate a benchmark.
    } while (!debugBench && i < maxLoops && (maxCoV map (_ < stats.cov) getOrElse false || i < minSampleLoops))
    val usedMemory = gcAndSnapshotUsedMemory() - memoryBefore

    if (!hasConsoleOutput)
      print(" ended benchmarking, name = %s, needed iterations = %d, time = " format (name, stats.iterations))
    val avgMs = stats.avg / math.pow(10, 6)
    val devStdMs = math.sqrt(stats.variance) / math.pow(10, 6)
    //The error of the measured average as an estimator of the average of the underlying random variable
    val stdErrMs = devStdMs / math.sqrt(stats.count)

    if (verbose) {
      if (hasConsoleOutput)
        print(">>> Name = %s, needed iterations = %d, time = " format (name, stats.iterations))
      println("(%,.3f +- %,.3f (stdErr = %,.3f)) ms; relative std.dev. %.3f, std.err. %.3f; extra memory consumption = %,d bytes" formatLocal (defaultLocale, avgMs, devStdMs, stdErrMs, devStdMs / avgMs, stdErrMs / avgMs, usedMemory))
    }
    //Format output for Jenkins' Measurement Plot plugin - https://wiki.jenkins-ci.org/display/JENKINS/Measurement+Plots+Plugin
    //println(<measurement><name>{name}</name><value>{avgMs}</value></measurement>) //Remove, that Jenkins plugin does not work.

    val nameToPrint = name.replace(';', '_')
    //Current log.
    logWriter.println("%s;%s;%s;%f;%f;%f" format (GitVersion.version, testDate, nameToPrint, avgMs, devStdMs, stdErrMs))
    logWriter.flush()
    //Detailed log.
    //Format output for R
    for (v <- if (printAllData) values else stats.samples) {
      rawDataLogWriter.println("%s;%s;%s;%d" format (GitVersion.version, testDate, nameToPrint, v))
    }
    rawDataLogWriter.flush()

    val benchmarkingEnd = System.nanoTime()
    println("Benchmarking required %d ms" format (benchmarkingEnd - benchmarkingBegin) / (1000 * 1000))
    //XXX: Maybe also return memoryBefore? Not for now.
    (ret, avgMs /*, memoryBefore*/)
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

object Benchmarking {
  val debugBench = false

  trait IVarianceCalc {
    var sum: Double = 0
    var sumSq: Double = 0
    //Total iterations whose results are kept (used to compute the avg. and std.dev.)
    def count: Int
    //Total iterations executed (counts the calls to update)
    def iterations: Int

    def avg = sum / count

    def variance = {
      val t = avg
      sumSq / count - t * t
    }

    def update(sample: Long)
    protected[this] def updateSumsForNewSample(sample: Long) {
      sum += sample
      sumSq += sample.asInstanceOf[Double] * sample
    }

    def cov =
      math.sqrt(variance) / avg

    def samples: Seq[Long]
  }

  class VarianceCalc(nSamples: Int) extends IVarianceCalc {
    val buf = ArrayBuffer.fill(nSamples)(0L)
    var idx = 0
    var iterations = 0

    //var count = 0
    override def count = math.min(nSamples, iterations)

    def samples: Seq[Long] = (buf take count).toList
    def update(sample: Long) {
      iterations += 1

      //Safe to do this
      sum -= buf(idx)
      sumSq -= buf(idx) * buf(idx)

      buf(idx) = sample

      updateSumsForNewSample(sample)

      idx = (idx + 1) % nSamples
    }
    override def cov =
      if (iterations < nSamples) 1 else super.cov
  }

  private val testDate = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(Calendar.getInstance.getTime)
  private val logPath = "LOSTestLog.csv"
  private val logWriter = new PrintWriter(new BufferedWriter(new FileWriter(logPath, true)))
  private val rawDataLogPath = "LOSTestLog-raw.csv"
  private val rawDataLogWriter = new PrintWriter(new BufferedWriter(new FileWriter(rawDataLogPath, true)))
  private var usedNames = Set[String]()

  //The JDK 1.6 ignores the grouping separator for the 1.6 locale.
  val defaultLocale = jUtil.Locale.UK //jUtil.Locale getDefault jUtil.Locale.Category.FORMAT
  jText.DecimalFormatSymbols getInstance defaultLocale setGroupingSeparator ' '
}
