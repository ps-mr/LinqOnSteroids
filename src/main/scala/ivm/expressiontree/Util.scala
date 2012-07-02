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
  def assertTypeAndRet[T](t: T) = t

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

//Much of the following code derives from scala.math.Ordering
object BetterPartialOrdering {
  trait PartialOrderingFromLteq[T] extends PartialOrdering[T] {
    override def tryCompare(x: T, y: T): Option[Int] =
      (lteq(x, y), lteq(y, x)) match {
        case (true, false) => Some(-1)
        case (false, true) => Some(1)
        case (true, true) => Some(0)
        case _ => None
      }
  }

  //Unused, untested
  trait PartialOrderingFromTryCompare[T] extends PartialOrdering[T] {
    override def lteq(x: T, y: T): Boolean = {
      //tryCompare(x, y).toRight(false).fold(identity, _ <= 0)
      //tryCompare(x, y).map(_ <= 0).getOrElse(false)
      tryCompare(x, y) match {
        case Some(res) => res <= 0
        case None => false
      }
    }
  }

  trait PartialOrderingExt[T] extends PartialOrdering[T] {
    outer =>
    def on[U](f: U => T): PartialOrderingExt[U] = new PartialOrderingExt[U] {
      override def lteq(x: U, y: U) = outer.lteq(f(x), f(y))
      override def tryCompare(x: U, y: U) = outer.tryCompare(f(x), f(y))
    }
    class Ops(lhs: T) {
      def <(rhs: T) = lt(lhs, rhs)
      def <=(rhs: T) = lteq(lhs, rhs)
      def >(rhs: T) = gt(lhs, rhs)
      def >=(rhs: T) = gteq(lhs, rhs)
      def equiv(rhs: T) = outer.equiv(lhs, rhs)
      /*
      //These option would have to return Option[T] for a PartialOrdering.
      def max(rhs: T): T = outer.max(lhs, rhs)
      def min(rhs: T): T = outer.min(lhs, rhs)
      */
    }
    implicit def mkOrderingOps(lhs: T): Ops = new Ops(lhs)
  }

  object PartialOrderingExt {
    def apply[T](implicit ord: PartialOrderingExt[T]) = ord

    object Implicits {
      implicit def infixOrderingOps[T](x: T)(implicit ord: PartialOrderingExt[T]): PartialOrderingExt[T]#Ops = new ord.Ops(x)
    }

    implicit def Tuple2[T1, T2](implicit ord1: Ordering[T1], ord2: Ordering[T2]): PartialOrderingExt[(T1, T2)] =
      new PartialOrderingExt[(T1, T2)] with PartialOrderingFromTryCompare[(T1, T2)] {
        //We have a certain result only if comparison on both sides don't contradict each other.
        override def tryCompare(x: (T1, T2), y: (T1, T2)): Option[Int] = {
          val compare1 = ord1.compare(x._1, y._1)
          val compare2 = ord2.compare(x._2, y._2)
          (compare1, compare2) match {
            case (a, b) if a == b => Some(a)
            case (a, 0) => Some(a)
            case (0, b) => Some(b)
            case _ => None
          }
        }
      }
  }
}
