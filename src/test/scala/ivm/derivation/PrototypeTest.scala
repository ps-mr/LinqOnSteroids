package ivm
package derivation
import expressiontree._
import Lifting._

import org.scalatest._

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */

class PrototypeTest extends FunSuite with Matchers with Prototype {
  val l = asExp(List(1, 2, 3))
  test("derive") {
    val query1 = l map (_ + 1)
    val query1p = derive(query1, l)
    query1p should be (FunSym(Fun((x: Exp[List[Int]]) => l map (_ + 1) union (x map (_ + 1)))))
    //println(query1p)
    val query2 = query1 map (_ - 1)
    val query2p = derive(query2, l)
    query2p should be (FunSym(Fun((x: Exp[List[Int]]) => l map (_ + 1) map (_ - 1) union (x map (_ + 1) map (_ - 1)))))
    //println(query2p)
  }

  test("derive more") {
    val query = l union List(1)
    //println(query)
    val queryp = derive(query, l)
    queryp should be (FunSym(Fun((x: Exp[List[Int]]) => query union x)))
    //println(queryp)
  }
}
