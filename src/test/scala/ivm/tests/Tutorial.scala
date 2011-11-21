package ivm
package tests

import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import org.junit.Test
import expressiontree._
import Lifting._
import optimization.Optimization
import collection.{mutable, TraversableView}

trait SmartIVMAPI {
  class ToQueryable[T](t: Traversable[T]) {
    def asQueryable: Exp[Traversable[T]] = Const(t)
  }
  implicit def toQueryable[T](t: Traversable[T]) = new ToQueryable(t)

  class Pimper[T](t: T) {
    //XXX: duplicates (with a better name) ToQueryable.asQueryable.
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

  //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
  def withExp[T, U](t: Exp[T])(f: T => U): Exp[U] = (f: Exp[T => U])(t)
  def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)

  //Used to force insertion of the appropriate implicit conversion - unlike ascriptions, one needn't write out the type
  //parameter of Exp here.
  def asExp[T](t: Exp[T]) = t
}

trait TestUtil {
  def showExp[T](t: Exp[T], message: String = "") {
    println("Query name: %s\n *\tstructure:\n\t%s\n *\tvalue:\n\t%s" format (message, t, t.interpret()))
  }

  def show(name: String, v: Any) {
    print(name + ": ")
    println(v)
  }

  /*
  def showInterp(name: String, v: Exp[_]) {
    show(name, v)
    show(name + ".interpret", v.interpret)
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

class Tutorial extends JUnitSuite with ShouldMatchersForJUnit with SmartIVMAPI with TestUtil {
  import Util._

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

  @Test def mistakesWithForComprehensions() {
    //Demonstrate a problem: here we need to explicitly convert the second collection; automatic conversions happen only
    val LibrariesAndHackersTest = for {
      lib <- testLibs.asSmartCollection //change 1
      libDev <- lib.developers
      dev <- testHackers //change 2 (not really needed here, but...)
    } yield (lib, dev)
    //Ugly type:
    assertType[Exp[Traversable[(ivm.expressiontree.Exp[Library], Developer)]]](LibrariesAndHackersTest)
    //checkResult(LibrariesAndHackersTest) //Can't be called
    val LibrariesAndHackersTest2 = for {
      lib <- testLibs.asSmartCollection //change 1
      libDev <- lib.developers
      dev <- testHackers //change 2 (not really needed here, but...)
    } yield asExp((lib, asExp(dev)))
    //Ugly type:
    assertType[Exp[Traversable[ivm.expressiontree.Exp[(Library, Developer)]]]](LibrariesAndHackersTest2)
  }

  @Test
  def moreTests() {
    println("testBug:")

    val i: Exp[Int] = 1
    show("i", i)
    val i1: Exp[Int] = 1
    show("i1", i1)
    //One of the syntaxes we want to support - both ones just fail, with "could not find implicit value for parameter cTTE: ivm.expressiontree.OpenEncoding.CanBuildExp[Int,ExpT]"
    //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5)
    //val a0: Exp[Traversable[Int]] = Seq(1, 2, 3, 5).toTraversable
    val a0: Exp[Seq[Int]] = Seq(1, 2, 3, 5)
    show("a0", a0)

    /*val a1 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - canBuildExp[Seq[Int]]: CanBuildExp[Seq[Int], Exp[Seq[Int]]] is preferred to canBuildExpTrav[Int, NumExp[Int]]: CanBuildExp[Traversable[Int], TraversableExp[Int]].
    show("a1", a1)
    val a2 = toExpTempl(Seq(1, 2, 3, 5).toTraversable) //Doesn't work well either
    show("a2", a2)
    val a3: Any = toExpTempl(Seq(1, 2, 3, 5)) //Works better
    show("a3", a3)
    val a4: Exp[Seq[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - that's equivalent to a1
    show("a4", a4)
    val a5: Exp[Traversable[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Works well apparently.
    show("a5", a5)
    val a6: TraversableExp[Int, NumExp[Int]] = Seq(1, 2, 3, 5) //This one obviously works.
    show("a6", a6)

    show("(like a3) toExpTempl(Seq(1, 2, 3, 5))", toExpTempl(Seq(1, 2, 3, 5)))
    //show("toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav))
    show("toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav))*/
  }

  @Test
  def testInadequate(c: Exp[Map[Int, Int]]) {
    val e: Exp[Map[Int, Int]] = c map (_ match {
      case Pair(a, b) => (a, b + 1) //Inadequate term, even if it's the first I wrote; it causes a crash
    })
    assertType[Exp[Map[Int, Int]]](e)
    showInterp("e", e)
  }

  @Test
  def testTraversableView(exp: Exp[Traversable[Int]]) {
    val a = exp.view
    showInterp("a", a)
    assertType[Exp[Traversable[Int]]](a)
    assertType[Exp[TraversableView[Int, Traversable[Int]]]](a)
    val b = a.map(_ + 1)
    assertType[Exp[Traversable[Int]]](b)
    // The underscore is due to the type of TraversableView.canBuildFrom. However, it doesn't really matter - see the
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
    assertType[Exp[TraversableView[(Int, Int), Map[Int, Int]]]](d5)
    showInterp("d5", d5)

    val d6 = d5 withFilter (ab => (ab._1 + ab._2 <= 4))
    assertType[Exp[TraversableView[(Int, Int), Map[Int, Int]]]](d6)
    showInterp("d6", d6)

    val forced = d6.force
    assertType[Exp[Map[Int, Int]]](forced)

    val d7 = c groupBy (ab => ab._2)
    assertType[Exp[Map[Int, Map[Int, Int]]]](d7)
    showInterp("d7", d7)

    val d8 = d7(4)
    assertType[Exp[Map[Int, Int]]](d8)
    showInterp("d8", d8)
  }

  @Test
  def testTraversable() {
    val data = Seq(1, 2, 2, 3, 5, 5, 3)
    val a: Exp[Seq[Int]] = data
    val a2 = data.asQueryable
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
  }

  @Test
  def main() {
    val a: Exp[Int] = 1
    val b = a + 2
    //With a smart signatures, type inference works:
    val c2 = withExp(1)(_ + 1)
    val c3 = withExpFunc(1)(_ + 1)

    println(a)
    println(b)
    println(c2)
    println(c3)
  }
}
