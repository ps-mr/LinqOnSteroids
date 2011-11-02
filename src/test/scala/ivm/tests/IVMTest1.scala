package ivm
package tests


import org.junit.Test
import collections.IncHashSet
import collection.mutable.HashSet
import org.scalatest.junit.{ShouldMatchersForJUnit, JUnitSuite}
import expressiontree.{Lifting, Exp, Queryable, IncrementalResult}

/**
 * User: pgiarrusso
 * Date: 31/10/2011
 */

class IVMTest1 extends JUnitSuite with ShouldMatchersForJUnit with IVMTestUtil {
  import Lifting._

  @Test
  def testIncremental() {
    val v = new IncHashSet[Int]
    v ++= Seq(1, 2, 3)

    println("v: " + v)
    val vPlusOne: IncHashSet[Int] = v.map(_ + 1) //canBuildFrom gives us the expected return type

    val vIncUpd = new IncrementalResult[Int](v)
    val v2: Queryable[Int, HashSet[Int]] = v //Queryable does not inherit from Traversable!
    println("v2.map(_ + 1): " + v2.map(_ + 1))
    println("v2.asQueryable.map(_ + 1): " + v2.asQueryable.map(_ + 1))
    println("v2.asCollection.map(_ + 1): " + v2.asCollection.map(_ + 1))
    val vQueryable: Exp[Traversable[Int]] = v.asQueryable
    assert(vQueryable == v)
    //val vQueryablePlusOne2: HashSet[Int] = vQueryable.map((i: Int) => i + 1) //gives error
    val vQueryablePlusOne = vQueryable.map(_ + 1)
    val vQueryablePlusOneIncRes = new IncrementalResult(vQueryablePlusOne)
    val vQueryablePlusOnePlusOne = new IncrementalResult(vQueryablePlusOne.map(_ + 1))
    //val vQueryablePOIRPlusOne = new IncrementalResult(vQueryablePlusOneIncRes.map(_ + 1)) //Compiles, but not what we want - the standard map is invoked!
    val vQueryablePOIRPlusOne = new IncrementalResult(vQueryablePlusOneIncRes.asQueryable.map(_ + 1))
    val vIncUpdTimes2 = new IncrementalResult(for (i <- vIncUpd.asQueryable) yield 2 * i)

    def out() {
      show("vIncUpd", vIncUpd)
      show("vIncUpdTimes2", vIncUpdTimes2)
      show("vQueryablePlusOnePlusOne", vQueryablePlusOnePlusOne)
      show("vQueryablePOIRPlusOne", vQueryablePOIRPlusOne)
    }
    out()
    v ++= Seq(4, 5, 6)
    out()

    // Check that redundant updates are handled correctly.
    // test actually - we need an union
    // operation for a proper test.
    v ++= Seq(4, 5, 6) //Now the inclusion count should be 1, not 2!
    out()
    v --= Seq(4, 5, 6) //Now the elements should already be removed!
    out()
    v --= Seq(4, 5, 6)
    out()
    v --= Seq(1, 2, 3)
    out()
    v.clear()
    out()
    v ++= Seq(1, 5, 7)
    out()
    v --= Seq(1, 5, 7)
    out()

    println("vQueryable: " + vQueryable)

    println("vPlusOne: " + vPlusOne)
    println("vQueryablePlusOne: " + vQueryablePlusOne)
    println("vQueryablePlusOne.interpret(): " + vQueryablePlusOne.interpret())

    val vColl: HashSet[Int] = v.asCollection //This is a simple upcast...
    println("vColl: " + vColl)
    assert(vColl == v)
    val vCollPlusOne: HashSet[Int] = vColl.map(_ + 1) //Here, the resulting object has actually HashSet as dynamic type.
    //Since vColl has a different static type, a different implicit is passed here.
    println("vCollPlusOne: %s, type: %s" format (vCollPlusOne, vCollPlusOne.getClass.getName))
  }
}