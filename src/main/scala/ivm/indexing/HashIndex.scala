package ivm
package indexing

import expressiontree.FuncExp
import scala.collection.immutable.HashMap
import expressiontree.QueryReifier

class HashIndex[T,S](it: QueryReifier[T], f: FuncExp[T,S] ) {
  val map : Map[S,Traversable[T]]= it.exec().groupBy(f.interpret())

}
