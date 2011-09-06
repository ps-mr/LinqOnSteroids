package ivm.expressiontree

import collection.TraversableView
import Lifting._

/**
 * User: pgiarrusso
 * Date: 6/9/2011
 */

object TravLifter {
  implicit def expToTraversableOps[T](t: Exp[Traversable[T]]) = new TraversableOps(t)
  implicit def toTraversableOps[T](t: Traversable[T]) = expToTraversableOps(t)

  case class Map[T, U](base: Exp[Traversable[T]], f: Exp[T => U]) extends BinaryOpExp[Traversable[T], T => U, Traversable[U]](base, f) {
    def copy(base: Exp[Traversable[T]], f: Exp[T => U]) = Map(base, f)
    override def interpret() = base.interpret map f.interpret()
  }

  case class FlatMap[T, U](base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) extends BinaryOpExp[Traversable[T], T => Traversable[U], Traversable[U]](base, f) {
    def copy(base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) = FlatMap(base, f)
    override def interpret() = base.interpret flatMap f.interpret()
  }

  /*case class WithFilter[T](base: Exp[Traversable[T]], f: Exp[T => Boolean]) extends Exp[Traversable[T]] {
    //XXX: Again the same problem with filtering - we cannot call withFilter.
    override def interpret = base.interpret.view filter f.interpret
  }*/
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
                                       resultSelector: FuncExp[(T, S), TResult]) extends Exp[Traversable[TResult]] {

    def children = Seq(colouter,colinner,outerKeySelector, innerKeySelector, resultSelector)
    def genericConstructor = (v) => Join(v(0).asInstanceOf[Exp[Traversable[T]]],
      v(1).asInstanceOf[Exp[Traversable[S]]],
      v(2).asInstanceOf[FuncExp[T, TKey]],
      v(3).asInstanceOf[FuncExp[S, TKey]],
      v(4).asInstanceOf[FuncExp[(T, S), TResult]])
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

  class TraversableOps[T](val t: Exp[Traversable[T]]) /*extends Exp[Traversable[T]]*/ {
    def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
      Map(this.t, FuncExp(f))

    def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]] =
      FlatMap(this.t, FuncExp(f))

    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
      WithFilter2(View(this.t), FuncExp(f))

    def union[U >: T](that: Exp[Traversable[U]]): Exp[Traversable[U]] =
      Union(this.t, that)

    def join[S,TKey,TResult](outercol: Exp[Traversable[S]],
                             outerKeySelector: Exp[T] => Exp[TKey],
                             innerKeySelector: Exp[S] => Exp[TKey],
                             resultSelector: Exp[(T, S)] => Exp[TResult]): Exp[Traversable[TResult]]
    = Join[T,S,TKey,TResult](this.t, outercol, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))
  }
}