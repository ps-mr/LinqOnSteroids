package ivm
package collections
import expressiontree.{Queryable, ObservableBuffer}

import collection.mutable.{Builder, BufferLike, ArrayBuffer, IndexedSeqOptimized}
import collection.generic.{SeqFactory, GenericTraversableTemplate}

/*
 * Just an experiment, don't use for real! We are going to support Sets first, not Buffers, as semantics and use cases
 * for them are unclear.
 */
class IncArrayBuffer[T] extends ArrayBuffer[T]
  with Queryable[T, ArrayBuffer[T]] with ObservableBuffer[T]
  with GenericTraversableTemplate[T, IncArrayBuffer]
  with BufferLike[T, IncArrayBuffer[T]]
  with IndexedSeqOptimized[T, IncArrayBuffer[T]]
  with Builder[T, IncArrayBuffer[T]]
{
  type Pub <: IncArrayBuffer[T] //Two different definitions of Pub are inherited, this one is a common subtype.
  override def companion = IncArrayBuffer
  override def result() = this
  override def newBuilder = new IncArrayBuffer[T]
}

object IncArrayBuffer extends SeqFactory[IncArrayBuffer] {
  override def newBuilder[U] = new IncArrayBuffer[U]
  implicit def canBuildFrom[U] = new GenericCanBuildFrom[U]
}
