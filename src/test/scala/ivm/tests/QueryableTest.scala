/*
package ivm
package tests

import expressiontree._

import org.scalatest.junit.JUnitSuite
import org.scalatest.junit.Matchers with AssertionsForJUnit
import collections.{IncHashSet, IncArrayBuffer}
import collection.mutable.{HashSet, ArrayBuffer}
import org.junit.Test

class QueryableTest extends JUnitSuite with Matchers with AssertionsForJUnit with IVMTestUtil {
  import Lifting._

  @Test
  def emptyIncHashSet() {
    val a = IncHashSet.empty
    a should not be (null)
    val b = IncHashSet()
    b should not be (null)
    b should be (a)
  }

  @Test
  def builderIncHashSet() {
    val c = IncHashSet.newBuilder[Int]
    c ++= Seq(1, 2, 3)
    val cRes: IncHashSet[Int] = c.result()
    val cRes2 = c.result()
    cRes should be (cRes2)
    cRes.getClass() should be (cRes2.getClass())
    cRes should be (Set(1, 2, 3))
    val d = IncHashSet(1, 2, 3)
    d should be (cRes)
  }

  @Test
  def emptyIncArrayBuffer() {
    val a = IncArrayBuffer.empty
    a should not be (null)
    val b = IncArrayBuffer()
    b should not be (null)
    b should be (a)
  }

  @Test
  def builderIncArrayBuffer() {
    val c = IncArrayBuffer.newBuilder[Int]
    c ++= Seq(1, 2, 3)
    val cRes: IncArrayBuffer[Int] = c.result()
    val cRes2 = c.result()
    cRes should be (cRes2)
    cRes should be (Seq(1, 2, 3))
    val d = IncArrayBuffer(1, 2, 3)
    d should be (cRes)
  }

  @Test
  def testIncArrayBuffer() {
    val v = new IncArrayBuffer[Int]
    v ++= Seq(1, 2, 3)

    println("v: " + v)
    val vPlusOne: IncArrayBuffer[Int] = v.map(_ + 1) // confusingly, this works
    val vQueryable: Exp[Traversable[Int]] = v.asQueryable
    assert(vQueryable == v)
    //val vQueryablePlusOne2: ArrayBuffer[Int] = vQueryable.map((i: Int) => i + 1) //gives error
    val vQueryablePlusOne: Exp[Traversable[Int]] = vQueryable.map(_ + 1)
    v ++= Seq(4, 5, 6) // Now, vPlusOne should be updated, shouldn't it?

    println("vQueryable: " + vQueryable)
    println("vPlusOne: " + vPlusOne)
    println("vQueryablePlusOne: " + vQueryablePlusOne)
    println("vQueryablePlusOne.eval: " + vQueryablePlusOne.eval)

    val vColl: ArrayBuffer[Int] = v.asCollection
    println("vColl: " + vColl)
    assert(vColl == v)
    val vCollPlusOne: ArrayBuffer[Int] = vColl.map(_ + 1)
    println("vCollPlusOne: " + vCollPlusOne)
  }

}
*/
