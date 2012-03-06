package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom

object ClassUtil {
  import java.{lang => jl}

  private val primitiveToBoxedMap = Map[Class[_], Class[_]](
    classOf[Byte] -> classOf[jl.Byte],
    classOf[Short] -> classOf[jl.Short],
    classOf[Char] -> classOf[jl.Character],
    classOf[Int] -> classOf[jl.Integer],
    classOf[Long] -> classOf[jl.Long],
    classOf[Float] -> classOf[jl.Float],
    classOf[Double] -> classOf[jl.Double],
    classOf[Boolean] -> classOf[jl.Boolean],
    classOf[Unit] -> classOf[jl.Void]
  )
  def primitiveToBoxed(classS: Class[_]) =
    /*if (cS <:< ClassManifest.AnyVal)
      primitiveToWrapper(cS.erasure)
    else
      cS.erasure*/
    primitiveToBoxedMap.getOrElse(classS, classS)
  def boxedErasure(cS: ClassManifest[_]) =
    /*if (cS <:< ClassManifest.AnyVal)
      primitiveToWrapper(cS.erasure)
    else
      cS.erasure*/
    primitiveToBoxed(cS.erasure)
}

object Util {
  def assertType[T](t: T) {}

  /**
   * Check that `v` and `typeParam` have the same type; if used as the returned expression, it also checks that the return type
   * also matches.
   * @param typeParam value whose type is the expected type
   * @param v value to return
   * @tparam T the type of `typeParam`; `v` must conform to this type.
   * @return `v`
   */
  def checkSameTypeAndRet[T](typeParam: T)(v: T): T = v

  //Precondition: classS must have been produced through primitiveToBoxed, because v will be boxed.
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
        ifInstanceOfBody[T, S](v, ClassUtil.boxedErasure(cS))
    }
  }
}
