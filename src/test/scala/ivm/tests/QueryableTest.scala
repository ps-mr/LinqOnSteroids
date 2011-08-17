package ivm
package tests

import collection.mutable.ArrayBuffer
import expressiontree.Lifting._
import expressiontree.{Queryable, QueryReifier, Lifting, ObservableBuffer}

object QueryableTest {
  def vect[T] = {
    //val res = new Vector[Int] with Queryable[Int, Vector[Int]] //Vector is final
    val res = new ArrayBuffer[Int] with Queryable[Int, ArrayBuffer[Int]] with ObservableBuffer[Int]
    res ++= Seq(1, 2, 3)
    res
  }
  def main(args: Array[String]) {
    import Lifting._
    val v = vect[Int]

    println("v: " + v)
    val vPlusOne: ArrayBuffer[Int] = v.map(_ + 1) // confusingly, this works
    v ++= Seq(4, 5, 6) // Now, vPlusOne should be updated, shouldn't it?
    println("vPlusOne: " + vPlusOne)
    val vQueryable: QueryReifier[Int] = v.asQueryable
    println("vQueryable: " + vQueryable)
    assert(vQueryable == v)
    //val vQueryablePlusOne2: ArrayBuffer[Int] = vQueryable.map((i: Int) => i + 1) //gives error
    val vQueryablePlusOne: QueryReifier[Int] = vQueryable.map(_ + 1)
    println("vQueryablePlusOne: " + vQueryablePlusOne)
    println("vQueryablePlusOne.interpret.exec(): " + vQueryablePlusOne.interpret.exec())

    val vColl: ArrayBuffer[Int] = v.asCollection
    println("vColl: " + vColl)
    assert(vColl == v)
    val vCollPlusOne: ArrayBuffer[Int] = vColl.map(_ + 1)
    println("vCollPlusOne: " + vCollPlusOne)
  }
}

