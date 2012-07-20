package ivm.collections

import collection.TraversableLike
import collection.generic.CanBuildFrom
import ivm.expressiontree.{ClassUtil, NoSub, YesSub, TypeHierarchyUtils, MaybeSub}


// contract: map must map a ClassTag[T] to a C[D[T]]
class TypeMapping[C[X] <: TraversableLike[X, C[X]], D[+_], Base](val map: Map[Class[_], C[D[Base]]], val subtypeRel: Map[Class[_], Set[Class[_]]], origColl: C[D[Base]]) {
  //TODO Problem with this implementation: instances of subtypes of T won't be part of the returned collection.
  //def getOld[T](implicit tmf: ClassTag[T]): C[D[T]] = map(ClassUtil.boxedErasure(tmf)).asInstanceOf[C[D[T]]]

  import TypeHierarchyUtils._

  //XXX reintroduce That here, maybe, for coherence. Not necessary for the paper, I believe (it can obviously be done, assuming the trick used for TypeFilterOps.when works here, too).
  //def get[T, That](implicit tmf: ClassTag[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], That]): That = {
  def get[T](implicit tmf: ClassTag[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): C[D[T]] = {
    m match {
      case v @ YesSub() =>
        //origColl map (_ map v.p.apply) //Try to apply the subtype relationship as a cast; to do this, we'd need D to be a Functor, and a witness of that to be passed.

        //For this to make sense, covariance of C and D is required, as in various other places.
        // However, C could be Set, which is not declared to be covariant. This is still safe in practice because if Der <: Gen,
        // in practice Set[Der] <: Set[Gen]: one can look up values of type Gen from a Set[Der] and will not find any.
        // One could declare the Set[T].contains method to accept Any, rather than T, and similarly for many other methods;
        // the ones which return a new Set could take a [B >: A] type parameter; this would allow making T a covariant
        // type parameter. This is not forbidden to ensure type safety, IMHO, but only to
        // reject some programs which are safe but stupid.
        // OTOH, if C is a mutable invariant collection, this could be a problem. We are only somewhat protected by the
        // fact that Scala mutable collections cannot be lifted implicitly, but there are many ways to circumvent that.
        origColl.asInstanceOf[C[D[T]]]
      //(cbf() ++= origColl.asInstanceOf[C[D[T]]]) result()
      case NoSub =>
        val clazz = ClassUtil.boxedErasure(tmf)
        get[T](clazz)
    }
  }

  def get[T](clazz: Class[_])(implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): C[D[T]] = {
    //XXX figure out a cleaner way to provide a default empty value. We rely here on
    //empty collections being castable to arbitrary types.
    val baseResult = map get clazz getOrElse cbf().result().asInstanceOf[C[D[Base]]]
    val coll = cbf(baseResult)
    coll ++= baseResult.asInstanceOf[C[D[T]]]
    for (t <- transitiveQuery(subtypeRel, clazz))
      coll ++= map(t).asInstanceOf[C[D[T]]] //TODO the same fix as above is required here - map(t) could fail!
    coll.result()
  }
}
