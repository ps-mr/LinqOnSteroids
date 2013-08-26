package ivm.expressiontree

import collection.generic.{CanBuildFrom, GenericTraversableTemplate}
import collection.{GenTraversableView, IterableLike, TraversableView, TraversableViewLike, TraversableLike}

trait TraversableOpsLangIntf {
  this: BaseExps =>

  abstract class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]]

  trait TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
    extends TraversableLikeOps[T, ViewColl]
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Rep[That]
  }

  trait TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]]
    extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]]

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Rep[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]]
}


trait TraversableOps extends TraversableOpsLangIntf {
  this: BaseExps =>
  //Removing this fixes the error.
  trait TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
    extends TraversableLikeOps[T, ViewColl]
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Exp[That]
  }

  //Removing this fixes the error.
  trait TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]]
    extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]] with super.TraversableViewOps[T, Repr]

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Exp[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]]
}

trait ForceOps {
  this: BaseExps with TraversableOps =>
  //Fixes the error.
  //this: BaseExps with TraversableOpsLangIntf =>

  sealed trait Forceable[T, Coll] {
    def force(t: Exp[Coll]): Exp[Traversable[T]]
  }
  //Note: type inference does not pick supertypes of arguments unless needed (i.e. if inferring T from t: T, the type
  //of T will be picked usually), therefore this implicit will be picked when needed. Note that since Forceable is invariant,
  //implicit resolution will not have other alternatives
  implicit def TraversableViewForceable[T]: Forceable[T, TraversableView[T, Traversable[_]]] = new Forceable[T, TraversableView[T, Traversable[_]]] {
    def force(t: Exp[TraversableView[T, Traversable[_]]]) =
      //expToTravViewExp2(t).force
      t.force //Fails compilation on 2.10.2. The same error appears on tests.
  }
}
