package ivm
package derivation
import expressiontree._
import Lifting._

import org.scalatest._

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */

class PrototypeTest extends FunSuite with Prototype {
  test("derive") {
    val l = asExp(List(1, 2, 3))
    println (derive(l map (_ + 1), l))
  }
}
