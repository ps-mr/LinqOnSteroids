package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom

object Util {
  def assertType[T](t: T) {}

  def ifInstanceOfBody[T, S](v: T, classS: Class[_]): Option[S] =
    if (v == null || !classS.isInstance(v))
      None
    else
      Some(v.asInstanceOf[S])

  object ExtraImplicits {
    class TraversableLike_GroupBySel_Op[T, Repr <: TraversableLike[T, Repr]](v: TraversableLike[T, Repr]) {
      def groupBySel[K, Rest, That](f: T => K, g: T => Rest)(implicit c: CanBuildFrom[Repr, Rest, That]): Map[K, That] =
        v.groupBy(f).map(v => (v._1, v._2.map(g)))
    }
    implicit def toTraversableLike_GroupBySel_Op[T, Repr <: TraversableLike[T, Repr]](v: TraversableLike[T, Repr]) = new TraversableLike_GroupBySel_Op(v)

    implicit def pimpInstanceOf[T](t: T) = new IfInstanceOfAble(t)
    class IfInstanceOfAble[T](v: T) {
      def ifInstanceOf[S](implicit cS: ClassManifest[S]): Option[S] =
        ifInstanceOfBody[T, S](v, IfInstanceOf.getErasure(cS))
    }
  }
}
