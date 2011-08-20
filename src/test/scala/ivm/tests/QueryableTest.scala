package ivm
package tests

//import expressiontree.{ObservableSet, Queryable, QueryReifier, Lifting, ObservableBuffer}
import expressiontree._
import collection.mutable.{SetLike, HashSet, Builder, BufferLike, IndexedSeqOptimized, ArrayBuffer}
import collection.generic.{MutableSetFactory, GenericSetTemplate, GenericCompanion, GenericTraversableTemplate}

/*
 * Just an experiment, don't use for real! We are going to support Sets first, not Buffers, as semantics and use cases
 * for them are unclear.
 */
class IncArrayBuffer[T] extends ArrayBuffer[T] with
    Queryable[T, ArrayBuffer[T]] with ObservableBuffer[T] with GenericTraversableTemplate[T, IncArrayBuffer] with
    BufferLike[T, IncArrayBuffer[T]] with IndexedSeqOptimized[T, IncArrayBuffer[T]] with Builder[T, IncArrayBuffer[T]] {
  override def companion = new GenericCompanion[IncArrayBuffer] {
    override def newBuilder[U] = new IncArrayBuffer[U]
  }
  override def result() = this
  override def newBuilder = new IncArrayBuffer[T]
}

// The below usage of Queryable is broken! For that, I really need to have IncrementalSet be a mixin instead of a
// decorator!


class IncHashSet[T] extends HashSet[T] with ObservableSet[T] with IncrementalSet[T]
   with Queryable[T, HashSet[T]]
   with /*extra templates:*/ GenericSetTemplate[T, IncHashSet] with SetLike[T, IncHashSet[T]] {
  type Pub <: IncHashSet[T] //Two different definitions of Pub are inherited, this one is a common subtype.
  override def companion = new MutableSetFactory[IncHashSet] {
    override def empty[U] = new IncHashSet[U]
    // XXX: without the above definition the code compiles, but empty is defined through newBuilder, and newBuilder
    // through empty, because one of the two is supposed to be overriden; not doing so results in infinite recursion.
    // TODO: write test for that.
  }
}

object QueryableTest {
  import Lifting._
  def testQueryable() {
    val v = new IncArrayBuffer[Int]
    v ++= Seq(1, 2, 3)

    println("v: " + v)
    val vPlusOne: ArrayBuffer[Int] = v.map(_ + 1) // confusingly, this works
    val vQueryable: QueryReifier[Int] = v.asQueryable
    assert(vQueryable == v)
    //val vQueryablePlusOne2: ArrayBuffer[Int] = vQueryable.map((i: Int) => i + 1) //gives error
    val vQueryablePlusOne: QueryReifier[Int] = vQueryable.map(_ + 1)
    v ++= Seq(4, 5, 6) // Now, vPlusOne should be updated, shouldn't it?

    println("vQueryable: " + vQueryable)
    println("vPlusOne: " + vPlusOne)
    println("vQueryablePlusOne: " + vQueryablePlusOne)
    println("vQueryablePlusOne.interpret.exec(): " + vQueryablePlusOne.interpret.exec())

    val vColl: ArrayBuffer[Int] = v.asCollection
    println("vColl: " + vColl)
    assert(vColl == v)
    val vCollPlusOne: ArrayBuffer[Int] = vColl.map(_ + 1)
    println("vCollPlusOne: " + vCollPlusOne)
  }

  def testIncremental() {
    val v = new IncHashSet[Int]
    v ++= Seq(1, 2, 3)
    //under ++= Seq(1, 2, 3) //XXX

    println("v: " + v)
    //val vPlusOne: IncHashSet[Int] = v.map(_ + 1) //Actually broken, we don't get IncHashSet here.
    val vPlusOne: HashSet[Int] = v.map(_ + 1) //XXX return type

    val vIncUpd = new IncrementalResult[Int](v)
    val vQueryable: QueryReifier[Int] = v.asQueryable  //XXX support this
    assert(vQueryable == v)
    //val vQueryablePlusOne2: ArrayBuffer[Int] = vQueryable.map((i: Int) => i + 1) //gives error
    val vQueryablePlusOne: QueryReifier[Int] = vQueryable.map(_ + 1)
    println("vIncUpd: " + vIncUpd)

    def out = {
      println()
      println("vIncUpd: " + vIncUpd)
      println("vIncUpd.interpret.exec(): " + vIncUpd.interpret.exec())
      println("vIncUpd.exec(): " + vIncUpd.exec())
      println("vIncUpd.inner.exec(): " + vIncUpd.inner.exec())
    }
    v ++= Seq(4, 5, 6)
    out

    // Check that redundant updates are handled correctly.
    // test actually - we need an union
    // operation for a proper test.
    v ++= Seq(4, 5, 6) //Now the inclusion count should be 1, not 2!
    out
    v --= Seq(4, 5, 6) //Now the elements should already be removed!
    out
    v --= Seq(4, 5, 6)
    out

    println("vQueryable: " + vQueryable)

    println("vPlusOne: " + vPlusOne)
    println("vQueryablePlusOne: " + vQueryablePlusOne)
    println("vQueryablePlusOne.interpret.exec(): " + vQueryablePlusOne.interpret.exec())

    val vColl: HashSet[Int] = v.asCollection
    println("vColl: " + vColl)
    assert(vColl == v)
    val vCollPlusOne: HashSet[Int] = vColl.map(_ + 1)
    println("vCollPlusOne: " + vCollPlusOne)
  }

  def main(args: Array[String]) {
    println("testQueryable")
    testQueryable()
    println("testIncremental")
    testIncremental()
  }
}

