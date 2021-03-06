package performancetests

import ivm._

import org.scalatest.Matchers
import org.scalatest.junit.{JUnitSuite, AssertionsForJUnit}
import org.junit.Test
import optimization.Optimization._
import expressiontree.Lifting._
import expressiontree.Exp

/**
 * User: pgiarrusso
 * Date: 12/10/2011
 */

class JoinCreationTest extends JUnitSuite with Matchers with AssertionsForJUnit with Benchmarking {
  @Test
  def performancetests() {
    //small performance test
    val testSize = 2000
    val c1: Exp[Traversable[Int]] = pure(Vector.range(1,testSize))
    val c2: Exp[Traversable[Int]] = pure(Vector.range(1,testSize))
    val n1 = Vector.range(1,testSize)
    val n2 = Vector.range(1,testSize)
    val notoptimized = for (k <- c1; k2 <- c2 if k ==# k2) yield k+k2
    benchMark("Native query") {
      val nativequery = for (k <- n1; k2 <- n2 if k.equals(k2)) yield k+k2
    }
    benchMark("Native query with primitive ==") {
      // The native query is significantly faster if we compare by k == k2 instead of k.equals(k2)
      val nativequery = for (k <- n1; k2 <- n2 if k == k2) yield k+k2
    }

    // Surprisingly, the notoptimized query becomes much slower if we implement
    // Eq.compare with == instead of equals. Klaus
    // RE: In my tests, I seldom get a difference, it is not significant, and seems to disappear
    // by enabling the Scala optimizer.
    val optimized = optimize(notoptimized)
    assert(
      benchMarkTime("Non optimized")(notoptimized.interpret().size)
      > (2 * benchMarkTime("Optimized")(optimized.interpret().size)))
  }
}
