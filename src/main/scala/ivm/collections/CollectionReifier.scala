package ivm
package collections

import expressiontree.ChildlessQueryReifier
import expressiontree.Const

//Reifier on collections (in particular, Traversable). Reifiers on other data sources are also possible.

class CollectionReifier[T](val innercol: Traversable[T]) extends Const(innercol) with ChildlessQueryReifier[T]
