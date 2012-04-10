package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree._
import Lifting._
import optimization.Optimization
import collection.{mutable, TraversableView}

trait TestUtil {
  def showExpNoVal[T](t: Exp[T], message: String = "") {
    print("\nQuery name: %s\n *\tstructure:\n\t%s\n" format (message, t))
  }

  def showExp[T](t: Exp[T], message: String = "") {
    showExpNoVal(t, message)
    print(" *\tvalue:\n\t%s\n\n" format t.interpret())
  }

  def show(name: String, v: Any) {
    print(name + ": ")
    println(v)
  }

  /*
  def showInterp(name: String, v: Exp[_]) {
    show(name, v)
    show(name + ".interpret()", v.interpret())
  }
  */

  def showInterp(name: String, v: Exp[_]) {
    showExp(v, name)
  }
}

/**
 * User: pgiarrusso
 * Date: 18/11/2011
 */

class Tutorial extends JUnitSuite with ShouldMatchersForJUnit with TestUtil {
  import Util.assertType

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
    def name = fmap(t)('Developer$name, _.name)
    def website = fmap(t)('Developer$website, _.website)
  }

  implicit def expToLibraryOps(t: Exp[Library]) = new LibraryOps(t)
  class LibraryOps(t: Exp[Library]) {
    def name = fmap(t)('Library$name, _.name)
    def depends = fmap(t)('Library$depends, _.depends)
    def developers = fmap(t)('Library$developers, _.developers)
  }
  //Code to be generated }}}

  @Test
  def basicQueries() {
    //Let us define a special collection for our purposes.
    val coll = Array.range(1, 10).asSmartCollection
    //Now, we can define queries on it
    val coll2 = for (c <- coll) yield c + 1
    showExp(coll2, "map with for-comprehension syntax")
    val coll3 = coll.map(c => c + 1)
    showExp(coll3, "map after desugaring")
  }

  @Test
  def exampleWithOptims() {
    val LibrariesAndHackersBase = for {
      lib <- testLibs
      libDev <- lib.developers
      dev <- testHackers
      if (libDev == dev.name)
    } yield (lib, dev)
    assertType[Seq[(Library, Developer)]](LibrariesAndHackersBase)
    println("Result of native query: " format LibrariesAndHackersBase)

    def checkResult(res: Exp[Traversable[(Library, Developer)]]) {
      LibrariesAndHackersBase should be (res.interpret())
      LibrariesAndHackersBase should be (Optimization.optimize(res).interpret())
    } //XXX: we want Seq here, not Traversable.

    val LibrariesAndHackersSmart = for {
      lib <- testLibs.asSmart //change 1
      libDev <- lib.developers
      dev <- testHackers.asSmart //change 2 (really needed here, unlike I thought!)
      if (libDev ==# dev.name) //if (libDev == dev.name) //Change 3: use '==#' instead of ==
    } yield (lib, dev)
    checkResult(LibrariesAndHackersSmart)
    showExp(LibrariesAndHackersSmart, "for comprehension over libraries and developers")
    showExp(Optimization.optimize(LibrariesAndHackersSmart), "optimized for comprehension over libraries and developers, using join")

    val LibrariesAndHackersSmartNested = for {
      libDevPair <- for (lib <- testLibs.asSmart; libDev <- lib.developers) yield (lib, libDev)
      dev <- testHackers.asSmart //change 2 (really needed here, unlike I thought!)
      if (libDevPair._2 ==# dev.name) //if (libDev == dev.name) //Change 3: use '==#' instead of ==
    } yield (libDevPair._1, dev)
    checkResult(LibrariesAndHackersSmartNested)

    showExp(LibrariesAndHackersSmartNested, "for comprehension over libraries and developers, using nested query")
    showExp(Optimization.optimize(LibrariesAndHackersSmartNested), "optimized for comprehension over libraries and developers, using join; this is what we actually want!")

    val LibrariesAndHackersExplicitJoin =
      (for (lib <- testLibs.asSmart; libDev <- lib.developers) yield (lib, libDev)).join(testHackers)(_._2, _.name, x => (x._1._1, x._2))
    showExp(LibrariesAndHackersExplicitJoin, "explicit join")
    showExp(Optimization.optimize(LibrariesAndHackersExplicitJoin), "optimized explicit join")
    checkResult(LibrariesAndHackersExplicitJoin)
    //XXX: Let us see that we can also perform interesting optimizations manually, or have them performed by an optimizer.
  }

  @Test def mistakesWithForComprehensions() {
    //Demonstrate a problem: here we need to explicitly convert the second collection; automatic conversions happen only
    val LibrariesAndHackersTest = for {
      lib <- testLibs.asSmart //change 1
      libDev <- lib.developers
      dev <- testHackers //change 2 (not really needed here, but...)
    } yield (lib, dev)
    //Ugly type:
    assertType[Exp[Seq[(ivm.expressiontree.Exp[Library], Developer)]]](LibrariesAndHackersTest)
    //checkResult(LibrariesAndHackersTest) //Can't be called
    val LibrariesAndHackersTest2 = for {
      lib <- testLibs.asSmart //change 1
      libDev <- lib.developers
      dev <- testHackers //change 2 (not really needed here, but...)
    } yield asExp((lib, asExp(dev)))
    //Not so ugly type anymore:
    assertType[Exp[Seq[(Library, Developer)]]](LibrariesAndHackersTest2)
  }

  @Test
  def moreTests() {
    val i: Exp[Int] = 1
    show("i", i)
    val i1: Exp[Int] = 1
    show("i1", i1)
    val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
    val a1: Exp[Seq[Int]] = Seq(1, 2, 3, 5)
    show("a0", a0)
    show("a1", a1)
  }

  def testInadequate(c: Exp[Map[Int, Int]]) {
    intercept[MatchError] {
      val e: Exp[Map[Int, Int]] = c map (_ match {
        case LiftTuple2(a, b) => (a, b + 1) //Inadequate term, even if it's the first I wrote; it causes a crash
      })
      assertType[Exp[Map[Int, Int]]](e)
      showInterp("e", e)
    }
  }

  def testTraversableView(exp: Exp[Traversable[Int]]) {
    val a = exp.view
    showInterp("a", a)
    assertType[Exp[Traversable[Int]]](a)
    assertType[Exp[TraversableView[Int, Traversable[Int]]]](a)
    val b = a.map(_ + 1)
    assertType[Exp[Traversable[Int]]](b)
    // The wildcard in the type is due to the type of TraversableView.canBuildFrom. However, it doesn't really matter - see the
    // type of forcedB
    assertType[Exp[TraversableView[Int, Traversable[_]]]](b)
    //assertType[Exp[TraversableView[Int, Traversable[Int]]]](b)
    showInterp("b", b)
    val forcedB = b.force
    showInterp("forcedB", forcedB)
    assertType[Exp[Traversable[Int]]](forcedB)

    val c = a.withFilter(_ <= 3)
    assertType[Exp[Traversable[Int]]](c)
    assertType[Exp[TraversableView[Int, Traversable[Int]]]](c)
    showInterp("c", c)

    val forcedC = c.force
    showInterp("forcedC", forcedC)
    assertType[Exp[Traversable[Int]]](forcedC)
  }

  def testNoMutableConst() {
    val mutableD = mutable.Seq(1)
    //XXX: As desired, thanks to noConstForMutableColl this code does not compile. It is unfortunate that we can't assert
    // that some code does not compile. Recheck when changing this code.
    //val mutExp: Exp[Traversable[Int]] = mutableD
  }

  @Test
  def testMap() {
    val c: Exp[Map[Int, Int]] = Map(1 -> 2, 2 -> 4, 3 -> 4)
    showInterp("c", c)
    // Type annotations on the results of map below are not needed to get the correct result, they just check that the
    // result has the correct type.
    //XXX: Implicit conversions are not used by the Scala compiler to fix pattern match errors.
    val d = c map (unliftPair(_) match {
      case (a, b) => (a, b + 1)
    })
    assertType[Exp[Map[Int, Int]]](d)
    showInterp("d", d)
    val d2 = c map (ab => (ab._1, ab._2 + 1))
    assertType[Exp[Map[Int, Int]]](d2)
    showInterp("d2", d2)
    val d3 = c map (ab => (ab._1 + ab._2))
    assertType[Exp[Iterable[Int]]](d3)
    showInterp("d3", d3)

    val d4 = c filter (ab => (ab._1 + ab._2 <= 4))
    assertType[Exp[Map[Int, Int]]](d4)
    showInterp("d4", d4)

    val d5 = c withFilter (ab => (ab._1 + ab._2 <= 4))
    assertType[Exp[Map[Int, Int]]](d5)
    showInterp("d5", d5)

    val d6 = d5 withFilter (ab => (ab._1 + ab._2 <= 4))
    assertType[Exp[Map[Int, Int]]](d6)
    showInterp("d6", d6)

    /*
    val d6view = d6.view
    assertType[Exp[TraversableView[(Int, Int), Map[Int, Int]]]](d6view)
    val forced = d6view.force
    assertType[Exp[Map[Int, Int]]](forced)
    */

    val d7 = c groupBy (ab => ab._2)
    assertType[Exp[Map[Int, Map[Int, Int]]]](d7)
    showInterp("d7", d7)

    val d8 = d7(4)
    assertType[Exp[Map[Int, Int]]](d8)
    showInterp("d8", d8)

    testInadequate(c)
  }

  @Test
  def testTraversable() {
    val data = Seq(1, 2, 2, 3, 5, 5, 3)
    val a: Exp[Seq[Int]] = data
    val a2 = data.asSmart
    assertType[Exp[Traversable[Int]]](a2) //assertType[Exp[Seq[Int]]](a2)
    val b1 = a.map(_ + 1)
    val b2 = a2.map(1 + _)
    val b3 = b1.map(2 + _)
    showInterp("b1", b1)
    showInterp("b2", b2)
    showInterp("b3", b3)
    val b4 = a groupBy identity
    assertType[Exp[Map[Int, Traversable[Int]]]](b4)
    showInterp("b4", b4)
    testTraversableView(a)
  }

  @Test
  def testSet() {
    val data = Set(1, 2, 3, 5)
    val a: Exp[Set[Int]] = data
    val a2 = data.asSmart
    assertType[Exp[Set[Int]]](a2)
    val b1 = a.map(_ + 1)
    val b2 = a2.map(1 + _)
    val b3 = b1.map(2 + _)
    showInterp("b1", b1)
    showInterp("b2", b2)
    showInterp("b3", b3)
    val b4 = a groupBy identity
    assertType[Exp[Map[Int, Set[Int]]]](b4)
    showInterp("b4", b4)
  }

  @Test
  def main() {
    val a: Exp[Int] = 1
    val b = a + 2
    //With a smart signatures, type inference works:
    val c2 = fmap(1)('plusOne, _ + 1)
    val c3 = withExpFunc(1)(_ + 1)

    println(a)
    println(b)
    println(c2)
    println(c3)
  }
}
