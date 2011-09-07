package ivm
package expressiontree

import Lifting._
import optimization.Optimization

import collection.{mutable, TraversableView}
import mutable.HashMap
import indexing.Index

/**
 * User: pgiarrusso
 * Date: 6/9/2011
 */

object TravLifter {
  implicit def expToTraversableOps[T](t: Exp[Traversable[T]]) = new TraversableWrapper(t)
  implicit def toTraversableOps[T](t: Traversable[T]) = expToTraversableOps(t)

  case class MapOp[T, U](base: Exp[Traversable[T]], f: Exp[T => U]) extends BinaryOpExp[Traversable[T], T => U, Traversable[U]](base, f) {
    def copy(base: Exp[Traversable[T]], f: Exp[T => U]) = MapOp(base, f)
    override def interpret() = base.interpret map f.interpret()
  }

  case class FlatMap[T, U](base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) extends BinaryOpExp[Traversable[T], T => Traversable[U], Traversable[U]](base, f) {
    def copy(base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) = FlatMap(base, f)
    override def interpret() = base.interpret flatMap f.interpret()
  }

  case class WithFilter[T](base: Exp[Traversable[T]], f: Exp[T => Boolean]) extends BinaryOpExp[Traversable[T], T => Boolean, Traversable[T]](base, f) {
    override def copy(base: Exp[Traversable[T]], f: Exp[T => Boolean]) = WithFilter(base, f)
    //XXX: Again the same problem with filtering - we cannot call withFilter because of its very generic result type.
    override def interpret = base.interpret.view filter f.interpret()
  }

  case class Union[T](lhs: Exp[Traversable[T]], rhs: Exp[Traversable[T]]) extends BinaryOpSymmExp[Traversable[T], Traversable[T]](lhs, rhs) {
    def copy(base: Exp[Traversable[T]], that: Exp[Traversable[T]]) = Union(base, that)
    override def interpret() = lhs.interpret ++ rhs.interpret
  }

  case class View[T](base: Exp[Traversable[T]]) extends UnaryOpExp[Traversable[T], TraversableView[T, Traversable[T]]](base) {
    override def copy(base: Exp[Traversable[T]]) = View(base)
    override def interpret() = base.interpret().view
  }

  case class WithFilter2[T](base: Exp[TraversableView[T, Traversable[T]]], f: Exp[T => Boolean]) extends BinaryOpExp[TraversableView[T, Traversable[T]], T => Boolean, Traversable[T]](base, f) {
    override def copy(base: Exp[TraversableView[T, Traversable[T]]], f: Exp[T => Boolean]) = WithFilter2(base, f)
    override def interpret() = base.interpret filter f.interpret()
  }

  case class Join[T, S, TKey, TResult](colouter: Exp[Traversable[T]],
                                       colinner: Exp[Traversable[S]],
                                       outerKeySelector: FuncExp[T, TKey],
                                       innerKeySelector: FuncExp[S, TKey],
                                       resultSelector: FuncExp[(T, S), TResult]) extends
                                       QuinaryOp[Exp[Traversable[T]],
                                         Exp[Traversable[S]],
                                         FuncExp[T, TKey], FuncExp[S, TKey], FuncExp[(T, S), TResult],
                                         Traversable[TResult]](colouter, colinner, outerKeySelector, innerKeySelector, resultSelector) {
    override def copy(colouter: Exp[Traversable[T]],
                                       colinner: Exp[Traversable[S]],
                                       outerKeySelector: FuncExp[T, TKey],
                                       innerKeySelector: FuncExp[S, TKey],
                                       resultSelector: FuncExp[(T, S), TResult]) = Join(colouter, colinner, outerKeySelector, innerKeySelector, resultSelector)

    override def interpret() = {
      // naive hash join algorithm
      val ci: Traversable[S] = colinner.interpret()
      val co: Traversable[T] = colouter.interpret()
      if (ci.size > co.size) {
        val map  = ci.groupBy(innerKeySelector.interpret())
        for (c <- co; d <- map(outerKeySelector.interpret()(c))) yield resultSelector.interpret()(c,d)
      } else {
        val map  = co.groupBy(outerKeySelector.interpret())
        for (c <- ci; d <- map(innerKeySelector.interpret()(c))) yield resultSelector.interpret()(d,c)
      }
    }
  }

  trait TraversableOps[T] {
    val underlying: Exp[Traversable[T]]
    def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
      MapOp(this.underlying, FuncExp(f))

    def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]] =
      FlatMap(this.underlying, FuncExp(f))

    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
      WithFilter(this.underlying, FuncExp(f))
      //WithFilter2(View(this.underlying), FuncExp(f))

    def union[U >: T](that: Exp[Traversable[U]]): Exp[Traversable[U]] =
      Union(this.underlying, that)

    def join[S,TKey,TResult](outercol: Exp[Traversable[S]],
                             outerKeySelector: Exp[T] => Exp[TKey],
                             innerKeySelector: Exp[S] => Exp[TKey],
                             resultSelector: Exp[(T, S)] => Exp[TResult]): Exp[Traversable[TResult]]
    = Join[T,S,TKey,TResult](this.underlying, outercol, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))
  }
  
  class TraversableWrapper[T](val underlying: Exp[Traversable[T]]) extends TraversableOps[T] {
    def asIndexable = new QueryReifier[T](underlying)
  }

  class HashIndex[T,S](it: Exp[Traversable[T]], f: FuncExp[T, S]) extends HashMap[S, Traversable[T]] with Index[S, Traversable[T]] {
    this ++= it.interpret().groupBy(f.interpret())
  }

  //XXX better define the usage.
  class QueryReifier[T](val t: Exp[Traversable[T]]) extends UnaryOpExp[Traversable[T], Traversable[T]](t) with TraversableOps[T] {
    //Bad idea - the QueryReifier node needs to be part of the expression tree.
    //override val underlying = t
    override val underlying = this
    override def copy(t: Exp[Traversable[T]]) = new QueryReifier(t)
    override def interpret() = t.interpret()

    val indexes: mutable.Map[FuncExp[T,_],HashIndex[T,_]] = HashMap()
    def addIndex[S](f: FuncExp[T,S]) {
      val nf = Optimization.normalize(f).asInstanceOf[FuncExp[T,S]]
      indexes += ((nf, new HashIndex(this,nf)))
    }
    def addIndex[S](f: Exp[T] => Exp[S]) {
      addIndex(FuncExp(f))
    }
  }
}
