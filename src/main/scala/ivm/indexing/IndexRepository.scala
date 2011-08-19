package ivm.indexing

import scala.collection.mutable.HashMap
import ivm.expressiontree.FuncExp

class IndexRepository extends  {
 private val indexes : HashMap[Traversable[Any] , HashMap[FuncExp[Nothing,Any], Index[Any,Any]]]  = new HashMap()

  def getIndex[T,S](col: Traversable[T], f: FuncExp[T,S] ) : Option[Index[T,S]] = {
    if (indexes.contains(col)) {
      val m = indexes(col)
      if (m.contains(f)) Some(m(f).asInstanceOf[Index[T,S]]) else None
    } else None
  }
}

object IndexRepository extends IndexRepository