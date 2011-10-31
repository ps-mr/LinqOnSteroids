package ivm.tests

import ivm.expressiontree.IncrementalResult
import org.scalatest.junit.ShouldMatchersForJUnit

/**
 * User: pgiarrusso
 * Date: 31/10/2011
 */

trait IVMTestUtil {
  this: ShouldMatchersForJUnit =>
  def show[T: Ordering](name: String, v: IncrementalResult[T]) {
    println()
    println("%s: after sorting\t\t%s, before\t\t%s; query:\n%s" format(name, v.toSeq.sorted, v, v.base))
    v.interpret() should be (v.base.interpret())
  }
}