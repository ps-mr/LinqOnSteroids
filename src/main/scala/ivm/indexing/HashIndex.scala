package ivm
package indexing

import scala.collection.immutable.HashMap
import expressiontree.{Exp, FuncExp}
import expressiontree.Lifting._

class HashIndex[T,S](it: Exp[Traversable[T]], f: FuncExp[T,S]) {
  val map : Map[S,Traversable[T]]= it.exec().groupBy(f.interpret())

}
