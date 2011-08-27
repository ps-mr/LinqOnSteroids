package ivm
package expressiontree

import scala.collection.mutable.HashMap
import scala.collection.mutable
import indexing.HashIndex
import optimization.Optimization

/*
 * TODO: we can move all these methods to Exp[T], by making them accept an implicit parameter only existing for
 * T = QueryReifier[U]. Or better, for T = Traversable[U] - that would replace
 * QueryReifier[U] by Exp[Traversable[U]]/Exp[TraversableView[U]]. How to make that extensible is not obvious.
 */
trait QueryReifierBase[T] extends Exp[QueryReifierBase[T]]  {
  this: QueryReifier[T] =>
  override def interpret() = this
  def exec(): Traversable[T]

  def map[U](f: Exp[T] => Exp[U]): QueryReifierBase[U] = Map[T,U](this, FuncExp(f))
  def withFilter(p: Exp[T] => Exp[Boolean]): QueryReifierBase[T] = WithFilter[T](this, FuncExp(p))
  //Causes test failures - the optimizer must still be adapted! But seemingly produces the same speedup
  //def withFilter(p: Exp[T]=>Exp[Boolean]) : QueryReifierBase[T] = new WithFilter[T](this.view, FuncExp(p)).force
  def flatMap[U](f: Exp[T] => Exp[QueryReifier[U]]): QueryReifierBase[U] = FlatMap[T,U](this, FuncExp(f))

  //Compute fix point of f applied to this collection. Useful for static analyses. Totally untested.
  def fix(f: Exp[Traversable[T]] => Exp[Traversable[T]]): Exp[QueryReifierBase[T]] = Fix[T](this, FuncExp(f))
  def fixWithReif(f: Exp[QueryReifier[T]] => Exp[QueryReifier[T]]): Exp[QueryReifierBase[T]] = FixWithReifiers[T](this, FuncExp(f))
  /*
   * Alternative type:
   * trait ReifyingTraversableLike[T, Repr[_]]
   * def fix(f: Exp[Repr[T] => Repr[T]]): Exp[Repr[T]]
   * This way, the type of fix gets refined for inheritors of ReifyingTraversableLike (say ReifyingSeqLike).
   */

  //XXX: the dummy parameter avoids view to override view in collections, when both are inherited (e.g. in Queryable).
  //The 'implicit' keyword allows to leave out the parameter list entirely.
  def view(implicit dummy: Boolean = false) = new View(this)
  def force = new Force(this)

  def join[S,TKey,TResult](outercol: QueryReifier[S],
                           outerKeySelector: Exp[T] =>Exp[TKey],
                           innerKeySelector: Exp[S]=>Exp[TKey],
                           resultSelector: Exp[(T,S)] => Exp[TResult]): QueryReifierBase[TResult]
    = Join[T,S,TKey,TResult](this, outercol, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))

  val indexes : mutable.Map[FuncExp[T,_],HashIndex[T,_]] = HashMap()
  def addIndex[S](f: FuncExp[T,S]) {
    val nf = Optimization.normalize(f).asInstanceOf[FuncExp[T,S]]
    indexes += ((nf, new HashIndex(this,nf)))
  }
  def addIndex[S](f: Exp[T] => Exp[S]) {
    addIndex(FuncExp(f))
  }
}

// Variant of QueryReifier, which also sends event to derived collections. Note that this is not reified!
// XXX: we forget to add mutation operations. But see Queryable and QueryableTest. So make this a trait which is mixed in
// by Queryable.
trait QueryReifier[T] extends QueryReifierBase[T] with MsgSeqPublisher[T] with Exp[QueryReifier[T]] {
  type Pub <: QueryReifier[T]
  override def map[U](f: Exp[T] => Exp[U]): QueryReifier[U] = {
    val res = new MapMaintainerExp[T, U](this, FuncExp(f))
    this subscribe res
    res
  }
  override def withFilter(p: Exp[T] => Exp[Boolean]): QueryReifier[T] = {
    val res = new WithFilterMaintainerExp[T](this, FuncExp(p))
    this subscribe res
    res
  }
  override def flatMap[U](f: Exp[T] => Exp[QueryReifier[U]]): QueryReifier[U] = {
    val res = new FlatMapMaintainerExp[T, U](this, FuncExp(f))
    this subscribe res
    res
  }
  //XXX add join, and add union
}

trait ChildlessQueryReifier[T] extends ChildlessExp[QueryReifier[T]] with QueryOp[T]
