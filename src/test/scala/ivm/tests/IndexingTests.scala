package ivm
package tests

import scala.collection.immutable.Vector
import optimization.Optimization._
import collections.CollectionReifier
import expressiontree.Lifting._
import expressiontree._
import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.ShouldMatchersForJUnit
import org.junit.Test
import java.util.Vector

//import org.junit.Test
import Benchmarking._
 
class IndexingTests extends JUnitSuite with ShouldMatchersForJUnit {
<<<<<<< Updated upstream
  val l: CollectionReifier[(Int,Int)] = new CollectionReifier( for (a <- Vector.range(1, 2000); b <- (Vector.range(1, 2000))) yield (a,b))
=======
  val l: CollectionReifier[(Int,Int)] = new CollectionReifier( for (a <- Vector.range(1,1000); b <- (Vector.range(1,1000))) yield (a,b))
>>>>>>> Stashed changes
  @Test
  def testIndexing1() {
    val q1 = for (c <- l if c._1 + c._2 + 7 is 12) yield c
    l.addIndex(p => p._2 + p._1 + 7)
    val q2 = optimizeIndexing(q1).asInstanceOf[QueryOp[_]]
<<<<<<< Updated upstream
    println(q1)
    println(q2)
    benchMark("original query", 20)(q1.exec())
    benchMark("optimized query", 20)(q2.exec())
=======
   println(q1)
   println(q2)
    // optimized query should be at least 100x faster
   assert( benchMark("original query")(q1.exec().size) >  100* benchMark("optimized query")(q2.exec().size))
>>>>>>> Stashed changes
  }
}
