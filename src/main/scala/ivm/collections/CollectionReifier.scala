package ivm
package collections

import expressiontree.{QueryReifier, Const}

//Reifier on collections (in particular, Traversable). Reifiers on other data sources are also possible.

class CollectionReifier[T: ClassManifest](val innercol: Traversable[T]) extends Const(innercol) with QueryReifier[T]
