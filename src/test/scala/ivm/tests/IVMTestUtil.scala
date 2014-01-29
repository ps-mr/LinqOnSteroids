package ivm.tests

/*
import ivm.expressiontree.IncrementalResult
import org.scalatest.junit.Matchers with AssertionsForJUnit

/**
 * User: pgiarrusso
 * Date: 31/10/2011
 */

trait IVMTestUtil {
  this: Matchers with AssertionsForJUnit =>
  def checkIncRes[T](coll: IncrementalResult[T]) {
    coll.eval should be (coll.base.eval)
  }

  def show[T: Ordering](name: String, coll: IncrementalResult[T]) {
    println()
    println("%s: after sorting\t\t%s, before\t\t%s; query:\n%s" format(name, coll.toSeq.sorted, coll, coll.base))
    checkIncRes(coll)
  }
}
*/
