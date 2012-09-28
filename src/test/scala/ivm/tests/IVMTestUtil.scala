package ivm.tests

/*
import ivm.expressiontree.IncrementalResult
import org.scalatest.junit.ShouldMatchersForJUnit

/**
 * User: pgiarrusso
 * Date: 31/10/2011
 */

trait IVMTestUtil {
  this: ShouldMatchersForJUnit =>
  def checkIncRes[T](coll: IncrementalResult[T]) {
    coll.interpret() should be (coll.base.interpret())
  }

  def show[T: Ordering](name: String, coll: IncrementalResult[T]) {
    println()
    println("%s: after sorting\t\t%s, before\t\t%s; query:\n%s" format(name, coll.toSeq.sorted, coll, coll.base))
    checkIncRes(coll)
  }
}
*/
