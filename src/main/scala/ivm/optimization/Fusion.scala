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
  this: SimplificationsOptimTransforms with InliningDefs =>
  private def buildMergedMaps[
    T, U, V, To <: Traversable[V] with TraversableLike[V, To]](coll: Exp[Traversable[T]],
                                                               f: FunSym[T, U], g: FunSym[U, V],
                                                               cbf: CanBuildFrom[Nothing, V, To with TraversableLike[V, To]]): Exp[To] =
    coll.map(x => letExp(f(x))(g))(collection.breakOut(cbf))

  val mergeMaps: Exp[_] => Exp[_] = {
    case Sym(m @ MapNode(Sym(MapNode(coll, f)), g)) =>
      //Since inner nodes were already optimized, coll will not be a MapNode node, hence we needn't call mergeMaps on the
      //result.
      buildMergedMaps(coll, f, g, m.c)
    case e => e
  }

  private def buildMergedFlatMap1[T, U, V](coll: Exp[Traversable[T]], f: FunSym[T, U], g: FunSym[U, Traversable[V]]) =
    coll flatMap (x => letExp(f(x))(g))

  val mergeFlatMaps: Exp[_] => Exp[_] = {
    case Sym(FlatMap(Sym(MapNode(coll, f)), g)) =>
      mergeFlatMaps(buildMergedFlatMap1(coll, f, g))
    case e => e
  }

  /*val mergeFilters: Exp[_] => Exp[_] = {
    case e@Sym(Filter(col, f)) =>
      stripViewUntyped(col) match {
        case Sym(Filter(col2, f2)) =>
          //No need to call mergeFilters again on the result, since the traversal is top-down.
          col2 withFilter {
            (x: Exp[_]) => And(f2(x), f(x))
          }
        case _ => e
      }
    case e => e
  }*/
  val mergeFilters2: PartialFunction[Exp[_], Exp[_]] = {
    case Sym(Filter(Sym(Filter(collection, pred2)), pred1)) =>
      //No need to call mergeFilters again on the result, since the traversal is top-down.
      collection withFilter (x => pred2(x) && pred1(x))
  }

  val mergeFilters: Exp[_] => Exp[_] = {
    case Sym(Filter(Sym(Filter(collection, pred2)), pred1)) =>
      //No need to call mergeFilters again on the result, since the traversal is top-down.
      collection withFilter (x => pred2(x) && pred1(x))
    case e => e
  }

  /*
   * Consider:
   * coll1 flatMap (x1 => coll2 map (x2 => (x1, x2)) filter (pair => test(pair._1))
   * Conceptually, here the filter might be hoisted because it only depends on x1. But that's not visible unless we
   * merge the map and the filter, fuse their functions, and extract the filter again with transformedFilterToFilter.
   */
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    case Sym(Filter(Sym(MapNode(coll, mapFun)), pred)) =>
      //We preserve sharing here with letExp; currently, subsequent stages will do indiscriminate inlining and replicate the map, but
      //the inliner will later be improved.
      coll flatMap Fun.makefun(letExp(mapFun.body)(Fun.makefun(if_#(pred.body) { Seq(asExp(pred.x)) } else_# (Seq.empty), pred.x).f), mapFun.x).f
    case e => e
  }

  val transformedFilterToFilter: Exp[_] => Exp[_] = {
    case Sym(FlatMap(coll, FunSym(fmFun@FuncExpBody(Sym(IfThenElse(test, thenBranch, Sym(elseBranch@ExpSeq(Seq())))))))) =>
      //Note that the function we build here might well turn out to be, after flatMapToMap, an identity function to remove
      //with removeIdentityMaps.
      //In particular, this will always be the case when calling transformedFilterToFilter right after
      //filterToTransformedFilter, or if the Filter was not transformed enough.
      coll filter Fun.makefun(test, fmFun.x).f flatMap Fun.makefun(thenBranch, fmFun.x).f
    case Sym(FlatMap(coll, FunSym(fmFun@FuncExpBody(Sym(FlatMap(Sym(IfThenElse(test, thenBranch, Sym(elseBranch@ExpSeq(Seq())))), fmFun2)))))) =>
      coll filter Fun.makefun(test, fmFun.x).f flatMap Fun.makefun(thenBranch flatMap fmFun2, fmFun.x).f
    //This case merges a transformed filter with a filter to allow the merged filter to be recognized by the remaining cases
    //during the rest of the upward traversal.
    //XXX Matching against thenBranch@ExpSeq(Seq(el)) instead of just thenBranch is restrictive, but probably helps.
    case Sym(Filter(Sym(IfThenElse(test, Sym(thenBranch@ExpSeq(Seq(el))), Sym(elseBranch@ExpSeq(Seq())))), pred)) =>
      (betaReduction orElse emptyTransform)(letExp(el)(elVal => if_# (test && pred(elVal)) { Seq(elVal) } else_# { Seq.empty }))
    case e => e
  }

  val filterToTransformedFilter: Exp[_] => Exp[_] = {
    case Sym(Filter(coll, pred)) =>
      coll flatMap (x => if_# (pred(x)) { Seq(x) } else_# { Seq.empty })
    case e => e
  }
}
