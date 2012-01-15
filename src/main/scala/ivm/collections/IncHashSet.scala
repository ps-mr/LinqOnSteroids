package ivm
package collections

import collection.GenSet
import collection.mutable.{SetLike, HashSet, Set}
import collection.generic.{GenericCompanion, CanBuildFrom, MutableSetFactory, GenericSetTemplate}
import expressiontree._


/*
//Failed attempt to make this trait typecheck
trait IncSetLike[T,
                 BaseRepr,
                 CC[X] <: IncSetLike[X, BaseRepr, CC] with BaseRepr with Set[X] /*with QueryReifier[X] with ObservableSet[X]*/
                ]
   extends ObservableSet[T]
   with Queryable[T, BaseRepr]
   with GenericSetTemplate[T, CC] with SetLike[T, CC[T]] {
  self: BaseRepr =>
  abstract override def companion: GenericCompanion[CC]
  //self: CC[T] with BaseRepr =>
  type Pub <: CC[T] //Two different definitions of Pub are inherited, this one is a common subtype.
}*/

//First attempt at definition - works, but IncHashSet has a long list of traits to implement.
/*trait IncSetLike[T, BaseRepr]
  extends ObservableSet[T] with Queryable[T, BaseRepr]
{
  this: BaseRepr =>
  type Pub <: IncSetLike[T, BaseRepr] //Two different definitions of Pub are inherited, this one is a common subtype.
}

class IncHashSet[T] extends HashSet[T] with IncSetLike[T, HashSet[T]]
  //Refine the result types of HashSet methods
  with SetLike[T, IncHashSet[T]]
  with GenericSetTemplate[T, IncHashSet]
{
  type Pub <: IncHashSet[T] //Two different definitions of Pub are inherited, this one is a common subtype.
  override def companion = IncHashSet
}*/


//Alternative definition. Here IncSetLike also inherits from all other needed traits, but gets much more
//hacky (see companion = null) and complex - see the parameter bounds, often there to satisfy the bounds from the
//Scala library. Should the Scala library change a bit, tis code will be very fragile.
trait IncSetLike[T, CCThis[X] <: Set[X] with SetLike[X, CCThis[X]] with GenSet[X], BaseRepr]
  extends ObservableSet[T] with Queryable[T, collection.Set, BaseRepr] with SetLike[T, CCThis[T]]
  with GenericSetTemplate[T, CCThis] with MsgSeqPublisher[collection.Set[T], IncSetLike[T, CCThis, BaseRepr]]
{
  this: BaseRepr with CCThis[T] =>
  //type Pub <: IncSetLike[T, CCThis, BaseRepr]
  //Here we have an abstract definition with the right type from GenericSetTemplate,
  //but also a concrete definition from Set with the "wrong" type GenericCompanion[Set]. Hence, we must define a body,
  //so we fake it.
  override def companion: GenericCompanion[CCThis] = null
}
// Note: How comes that the above definition of IncSetLike does not break down? Now we restricted the scope of the
// publish method, by inheriting a stricter definition of MsgSeqPublisher. How come that ObservableSet can still publish
// messages of type Seq[Message[Traversable[T]]] ? See discussion about IncrementalResult

class IncHashSet[T] extends HashSet[T]
   with IncSetLike[T, IncHashSet, HashSet[T]] with MsgSeqPublisher[collection.Set[T], IncHashSet[T]]
{
  //type Pub <: IncHashSet[T] //This definition is not required but simplifies the definition of Pub
  override def companion = IncHashSet
}

// Next TODO: try to hide from the public interface of IncHashSet the methods from HashSet, so that people must call
// asCollection. Not sure whether it's a good idea. A possibly more sensible alternative is to hide methods from
// Exp[Traversable[T]], so that people have to call asQueryable to get them. Indeed, the type Queryable provides only
// methods from Exp[T], and one needs to call asCollection to access methods from the underlying collection.

object IncHashSet extends MutableSetFactory[IncHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IncHashSet[A]] = setCanBuildFrom[A]
  override def empty[A] = new IncHashSet[A]
  // Without the above definition the code compiles, but empty is defined through newBuilder, and newBuilder
  // through empty, because one of the two is supposed to be overriden; not doing so results in infinite recursion.
  // Thus there's a test for that in QueryableTest.emptyIncHashSet.
}

