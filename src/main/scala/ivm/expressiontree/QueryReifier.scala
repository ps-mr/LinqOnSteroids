package ivm
package expressiontree

import scala.collection.mutable.HashMap
import scala.collection.mutable
import optimization.Optimization

/*
 * TODO: we can move all these methods to Exp[T], by making them accept an implicit parameter only existing for
 * T = QueryReifier[U]. Or better, for T = Traversable[U] - that would replace
 * QueryReifier[U] by Exp[Traversable[U]]/Exp[TraversableView[U]]. How to make that extensible is not obvious.
 */
trait QueryReifierBase[T] extends Exp[Traversable[T]]  {
  this: QueryReifier[T] =>
  /*
  //Compute fix point of f applied to this collection. Useful for static analyses. Totally untested.
  def fix(f: Exp[Traversable[T]] => Exp[Traversable[T]]): Exp[QueryReifierBase[T]] = Fix[T](this, FuncExp(f))
  def fixWithReif(f: Exp[QueryReifier[T]] => Exp[QueryReifier[T]]): Exp[QueryReifierBase[T]] = FixWithReifiers[T](this, FuncExp(f))
  */
  /*
   * Alternative type:
   * trait ReifyingTraversableLike[T, Repr[_]]
   * def fix(f: Exp[Repr[T] => Repr[T]]): Exp[Repr[T]]
   * This way, the type of fix gets refined for inheritors of ReifyingTraversableLike (say ReifyingSeqLike).
   */

  //XXX: the dummy parameter avoids view to override view in collections, when both are inherited (e.g. in Queryable).
  //The 'implicit' keyword allows to leave out the parameter list entirely.
  def view(implicit dummy: Boolean = false) = new View[T, Traversable[T]](this)
}


// Variant of QueryReifier, which also sends event to derived collections. Note that this is not reified!
// XXX: we forget to add mutation operations. But see Queryable and QueryableTest. So make this a trait which is mixed in
// by Queryable.
trait QueryReifier[T] extends QueryReifierBase[T] with MsgSeqPublisher[T] with Exp[Traversable[T]] {
  type Pub <: QueryReifier[T]
  //subscription is a side effect, but query composition requires the passed functions to be pure.
  //In particular, when we pass Var inside a query, we execute these operations, but the results must just be inspected,
  //they must not become listeners - especially because they contain Var nodes.
  /*override def map[U](f: Exp[T] => Exp[U]): QueryReifier[U] =
    new MapOpMaintainerExp[T, U](this, FuncExp(f))
  override def withFilter(p: Exp[T] => Exp[Boolean]): QueryReifier[T] =
    new WithFilterMaintainerExp[T](this, FuncExp(p))
  override def flatMap[U](f: Exp[T] => Exp[QueryReifier[U]]): QueryReifier[U] =
    new FlatMapMaintainerExp[T, U](this, FuncExp(f))
  //XXX add join, and add union
  */
}

trait ChildlessQueryReifier[T] extends NullaryExpTrait[Traversable[T]] with QueryReifier[T]
