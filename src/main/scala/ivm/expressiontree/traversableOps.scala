package ivm.expressiontree

import collection.generic.CanBuildFrom
import collection.TraversableView

trait TraversableOps {
  type Rep[T]

  //Removing the implicit parameter in both fixes the crash, removing it into one only gives a valid compiler error.
  trait TraversableViewLikeOpsDup1[T, Repr] {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Rep[That]
  }

  trait TraversableViewLikeOpsDup2[T, Repr] {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Rep[That]
  }

  trait TraversableViewOps[T, Repr]
    //Removing either supertype fixes the error:
    extends TraversableViewLikeOpsDup1[T, Repr]
    with    TraversableViewLikeOpsDup2[T, Repr]

  implicit def expToTravViewExp2[T, C[X]](t: Rep[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]]

  def force[T](t: Rep[TraversableView[T, Traversable[_]]]) =
    //expToTravViewExp2(t).force
    t.force //Fails compilation on 2.10.2.
}
