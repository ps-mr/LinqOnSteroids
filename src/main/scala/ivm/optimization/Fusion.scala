package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._
import collection.generic.CanBuildFrom
import collection.TraversableLike

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait Fusion {
  private def buildMergedMaps[
    T, U, V, To <: Traversable[V] with TraversableLike[V, To]](coll: Exp[Traversable[T]],
                                                               f: Fun[T, U], g: Fun[U, V],
                                                               cbf: CanBuildFrom[Nothing, V, To with TraversableLike[V, To]]): Exp[To] =
    coll.map(x => letExp(f(x))(g))(collection.breakOut(cbf))

  val mergeMaps: Exp[_] => Exp[_] = {
    case m @ MapNode(MapNode(coll, f), g) =>
      //Since inner nodes were already optimized, coll will not be a MapNode node, hence we needn't call mergeMaps on the
      //result.
      buildMergedMaps(coll, f, g, m.c)
    case e => e
  }

  private def buildMergedFlatMap1[T, U, V](coll: Exp[Traversable[T]], f: Fun[T, U], g: Fun[U, Traversable[V]]) =
    coll flatMap (x => letExp(f(x))(g))

  val mergeFlatMaps: Exp[_] => Exp[_] = {
    case FlatMap(MapNode(coll, f), g) =>
      mergeFlatMaps(buildMergedFlatMap1(coll, f, g))
    case e => e
  }

  /*val mergeFilters: Exp[_] => Exp[_] = {
    case e@Filter(col, f) =>
      stripViewUntyped(col) match {
        case Filter(col2, f2) =>
          //No need to call mergeFilters again on the result, since the traversal is top-down.
          col2 withFilter {
            (x: Exp[_]) => And(f2(x), f(x))
          }
        case _ => e
      }
    case e => e
  }*/
  val mergeFilters: Exp[_] => Exp[_] = {
    case Filter(Filter(col2, f2), f) =>
      //No need to call mergeFilters again on the result, since the traversal is top-down.
      col2 withFilter (x => And(f2(x), f(x)))
    case e => e
  }

  //Fuse multiple views
  val mergeViews: Exp[_] => Exp[_] = {
    case View(coll@View(_)) =>
      coll
    case e => e
  }

  /*
   * Consider:
   * coll1 flatMap (x1 => coll2 map (x2 => (x1, x2)) filter (pair => test(pair._1))
   * Conceptually, here the filter might be hoisted because it only depends on x1. But that's not visible unless we
   * merge the map and the filter, fuse their functions, and extract the filter again with transformedFilterToFilter.
   */
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    case Filter(MapNode(coll, mapFun), pred) =>
      //We preserve sharing here with letExp; currently, subsequent stages will do indiscriminate inlining and replicate the map, but
      //the inliner will later be improved.
      coll flatMap Fun.makefun(letExp(mapFun.body)(Fun.makefun(if_#(pred.body)(Seq(pred.x)) else_# (Seq.empty), pred.x)), mapFun.x)
    case e => e
  }

  val transformedFilterToFilter: Exp[_] => Exp[_] = {
    case FlatMap(coll, fmFun@FuncExpBody(IfThenElse(test, thenBranch, elseBranch@ExpSeq(Seq())))) =>
      coll filter Fun.makefun(test, fmFun.x) flatMap Fun.makefun(thenBranch, fmFun.x)
    case FlatMap(coll, fmFun@FuncExpBody(FlatMap(IfThenElse(test, thenBranch, elseBranch@ExpSeq(Seq())), fmFun2))) =>
      coll filter Fun.makefun(test, fmFun.x) flatMap Fun.makefun(thenBranch flatMap fmFun2, fmFun.x)
    case e => e
  }

  val filterToTransformedFilter: Exp[_] => Exp[_] = {
    case Filter(coll, pred) =>
      coll flatMap (x => if_# (pred(x)) { Seq(x) } else_# { Seq.empty })
    case e => e
  }
}
