package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom

object Util {
  def assertType[T](t: T) {}

  class TraversableLike_GroupBySel_Op[T, Repr <: TraversableLike[T, Repr]](v: TraversableLike[T, Repr]) {
    def groupBySel[K, Rest, That](f: T => K, g: T => Rest)(implicit c: CanBuildFrom[Repr, Rest, That]): Map[K, That] =
      v.groupBy(f).map(v => (v._1, v._2.map(g)))
  }
  implicit def toTraversableLike_GroupBySel_Op[T, Repr <: TraversableLike[T, Repr]](v: TraversableLike[T, Repr]) = new TraversableLike_GroupBySel_Op(v)
}
