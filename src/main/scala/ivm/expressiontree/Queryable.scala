package ivm
package expressiontree

import collections.CollectionReifier
import collection.mutable.ArrayBuffer

/**
 * Trait to mix in with Scala collections to make them queryable.
 * An important point is that the original collection becomes queryable -
 * that is, that object identity is preserved.
 *
 * Key idea: make the same collection instance queryable.
 * Only a prototype.
 * Missing features:
 * - Lazy execution should return a more specific interface.
 *
 * - The built collection should possibly offer stronger interfaces.
 *   Relevant ones:
 *   - Traversable
 *   - Iterable (multiple iterators at the same time, looks easy)
 *   - Seq (sequential order is respected).
 *   - LinearSeq (linear access Seq)
 *   - IndexedSeq (random access Seq)
 * - When we execute the query, we should probably also register a notifier to update the generated collection.
 *
 */


//Probably Repr is also needed, especially if the produced Iterable must also offer stronger
//interfaces.
trait Queryable[T, Repr] extends ChildlessQueryReifier[T] {
  self : scala.collection.Traversable[T] with Repr =>
  def asQueryable: QueryReifier[T] = this
  def asCollection: Repr = this
  override def exec(isLazy: Boolean) = this
  //This allows selecting early how the query is to be executed.
  // The alternative is to choose between exec(isLazy = true) and exec(isLazy = false)
  //def asQueryableLazy: Iterable[T] = new CollectionReifier[T](this.view)
}

object QueryableTest {
  def vect[T] = {
    //val res = new Vector[Int] with Queryable[Int, Vector[Int]] //Vector is final
    val res = new ArrayBuffer[Int] with Queryable[Int, ArrayBuffer[Int]]
    res ++= Seq(1, 2, 3)
    res
  }
  def main(args: Array[String]) {
    import Lifting._
    val v = vect[Int]

    println("v: " + v)
    val vPlusOne: ArrayBuffer[Int] = v.map(_ + 1) // confusingly, this works
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

// vim: set ts=4 sw=4 et:
