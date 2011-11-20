package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree._
import Lifting._
import optimization.Optimization

trait SmartIVMAPI {
  class Pimper[T](t: T) {
    def asSmartCollection = t: Exp[T]
  }
  implicit def lift[T](t: T) = new Pimper(t)

  class ArrayPimper[T](t: Array[T]) {
    def asSmartCollection = t: Exp[Seq[T]]
  }
  implicit def lift2[T](t: Array[T]) = new ArrayPimper(t)
  //Either we use ArrayPimper, or we add the final cast to TraverableOps[T] here.
  //Since this is an implicit conversion, we can't just return Exp[Seq[T]] and rely on an additional implicit conversion to supply lifted collection methods.
  //implicit def expArrayToExpSeq[T](x: Exp[Array[T]]) = onExp(x)('castToSeq, x => x: Seq[T]): TraversableOps[T]

  class Materializable[T](t: Exp[Traversable[T]]) {
    def materialize = new IncrementalResult(t)
  }
  implicit def toMaterializable[T](t: Exp[Traversable[T]]) = new Materializable(t)
}

trait TestUtil {
  def showExp[T](t: Exp[T], message: String = "") {
    println("Query name: %s\n *\tstructure:\n\t%s\n *\tvalue:\n\t%s" format (message, t, t.interpret()))
  }  
}
/**
 * User: pgiarrusso
 * Date: 18/11/2011
 */

class Tutorial extends JUnitSuite with ShouldMatchersForJUnit with SmartIVMAPI with TestUtil {
  /*
   * This tutorial introduces the features of our DSEL for queries on collections.
   * We show
   * - how to express queries over collections
   * - how we get accurate result types while using little code to redefine collection types.
   * - results of optimizations, starting from flatMap to join transformation (XXX: how is that called?)
   * - very important feature: optimizations are expressible within the same language
   * - definition of first-class indexes, and their manual use
   * - automatic use of first-class indexes through the optimizer
   * - type indexes
   * - how we support incremental view maintenance
   */
  case class Developer(name: String, website: String)
  case class Library(name: String, depends: Seq[Library], developers: Seq[String]) //To introduce the need for a join,
  // I altered this definition to refer to developers by name, instead of by a direct pointer to the developer itself.
  // I still preserve the original syntax (for now) with this implicit conversion (yes, it's ugly).
  implicit def devToString(d: Developer) = d.name

  val devHacker = Developer("A hacker", "")
  val testHackers = Seq(devHacker)

  val libBase = Library("base", Seq(), Seq(devHacker))
  val libParsec = Library("parsec", Seq(libBase), Seq(devHacker))
  val testLibs = Seq(libParsec, libBase)

  //Code to be generated {{{
  implicit def expToDeveloperOps(t: Exp[Developer]) = new DeveloperOps(t)
  class DeveloperOps(t: Exp[Developer]) {
    def name = onExp(t)('name, _.name)
    def website = onExp(t)('website, _.website)
  }

  implicit def expToLibraryOps(t: Exp[Library]) = new LibraryOps(t)
  class LibraryOps(t: Exp[Library]) {
    def name = onExp(t)('name, _.name)
    def depends = onExp(t)('depends, _.depends)
    def developers = onExp(t)('developers, _.developers)
  }
  //Code to be generated }}}
  
  @Test
  def basicQueries() {
    //Let us define a special collection for our purposes.
    val coll = Array.range(1, 10).asSmartCollection
    //Now, we can define queries on it
    val coll2 = coll.map(_ + 1)
    showExp(coll2, "map")
    val coll3 = for (c <- coll) yield c + 1
    showExp(coll3, "map with for-comprehension syntax")
  }
  @Test
  def exampleWithOptims() {
    val LibrariesAndHackersBase = for {
      lib <- testLibs
      libDev <- lib.developers
      dev <- testHackers
      if (libDev == dev.name)
    } yield (lib, dev)
    println(LibrariesAndHackersBase)
    def checkResult(res: Exp[Traversable[(Library, Developer)]]) {
      LibrariesAndHackersBase should be (res.interpret())
      LibrariesAndHackersBase should be (Optimization.optimize(res).interpret())
    } //XXX: we want Seq here, not Traversable.

    val LibrariesAndHackersTest = for {
      lib <- testLibs.asSmartCollection //change 1
      libDev <- lib.developers
      dev <- testHackers //change 2 (not really needed here, but...)
    } yield (lib, dev)

    val LibrariesAndHackersSmart = for {
      lib <- testLibs.asSmartCollection //change 1
      libDev <- lib.developers
      dev <- testHackers.asSmartCollection //change 2 (actually really needed here!)
      if (libDev is dev.name) //if (libDev == dev.name) //Change 3: use 'is' instead of ==
    } yield (lib, dev)
    checkResult(LibrariesAndHackersSmart)
    showExp(LibrariesAndHackersSmart, "for comprehension over libraries and developers")
    showExp(Optimization.optimize(LibrariesAndHackersSmart), "optimized for comprehension over libraries and developers, using join")

    val LibrariesAndHackersSmartNested = for {
      libDevPair <- for (lib <- testLibs.asSmartCollection; libDev <- lib.developers) yield (lib, libDev)
      dev <- testHackers.asSmartCollection //change 2 (actually really needed here!)
      if (libDevPair._2 is dev.name) //if (libDev == dev.name) //Change 3: use 'is' instead of ==
    } yield (libDevPair._1, dev)
    checkResult(LibrariesAndHackersSmartNested)

    showExp(LibrariesAndHackersSmartNested, "for comprehension over libraries and developers, using nested query")
    showExp(Optimization.optimize(LibrariesAndHackersSmartNested), "optimized for comprehension over libraries and developers, using join; this is what we actually want!")

    val LibrariesAndHackersExplicitJoin =
      (for (lib <- testLibs.asSmartCollection; libDev <- lib.developers) yield (lib, libDev)).join(testHackers)(_._2, _.name, x => (x._1._1, x._2))
    showExp(LibrariesAndHackersExplicitJoin, "explicit join")
    showExp(Optimization.optimize(LibrariesAndHackersExplicitJoin), "optimized explicit join")
    checkResult(LibrariesAndHackersExplicitJoin)
    //Let us do the same on a Set, and get a Set out! XXX TODO
    //TODO: paste SimpleOpenEncoding here
    //Let us see that we can also perform interesting optimizations manually, or have them performed by an optimizer.
  }
}