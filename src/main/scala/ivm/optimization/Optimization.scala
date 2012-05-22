package ivm
package optimization

import expressiontree._
import Lifting._
import Numeric.Implicits._
import annotation.tailrec
import collection.mutable.Stack
import collection.TraversableLike
import OptimizationUtil._


object Optimization {
  import scala.collection.mutable.Map

  val subqueries: Map[Exp[_], Any] = Map.empty

  def resetSubqueries() = subqueries.clear()
  def addIndex[T](_query: UnconvertedExp[Exp[T]], res: Option[T] = None) {
    val query = OptimizationTransforms.stripViewUntyped(_query.v)
    val optquery = optimizeIdx(query)
    val intQuery = res match {
      case Some(v) => v
      case None => optquery.interpret() //XXX: what if query is an incrementally maintained collection? We don't want to call interpret() again!
    }


    //Let us ensure that both the unoptimized and the optimized version of the query are recognized by the
    // optimizer. TODO: Reconsider again whether this is a good idea.
    subqueries += normalize(query) -> intQuery
    subqueries += normalize(optquery) -> intQuery
  }

  def removeIndex[T](_query: UnconvertedExp[Exp[T]]) {
    val query = _query.v
    subqueries -= normalize(query)
    subqueries -= normalize(optimize(query))
  }

  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToJoin)

  def cartProdToAntiJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToAntiJoin)

  def reassociateOps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.reassociateOps)

  def mergeMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeMaps)

  def mergeViews[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeViews)

  def sizeToEmpty[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.sizeToEmpty)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = mergeViews(exp.transform(OptimizationTransforms.mergeFilters))

  def removeTrivialFilters[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeTrivialFilters)

  def simplifyConditions[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.simplifyConditions)

  def simplifyFilters[T](exp: Exp[T]): Exp[T] = removeTrivialFilters(simplifyConditions(exp))

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeIdentityMaps)

  def toTypeFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.toTypeFilter)

  def hoistFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.hoistFilter)

  def removeRedundantOption[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeRedundantOption)

  def mapToFlatMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mapToFlatMap)

  def flatMapToMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.flatMapToMap)

  def filterToWithFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.filterToWithFilter)

  def letTransformerTrivial[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.letTransformerTrivial)

  def letTransformerUsedAtMostOnce[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.letTransformerUsedAtMostOnce)

  def letTransformer[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.letTransformer)

  //This should be called after any sort of inlining, including for instance map fusion.
  def betaDeltaReducer[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.betaDeltaReducer)

  //Unsafe yet, hence not used!
  def existsUnnester[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.existsUnnester)

  def generalUnnesting[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.generalUnnesting)

  def simplifyForceView[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.simplifyForceView)

  def mergeFilterWithMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFilterWithMap)

  def transformedFilterToFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.transformedFilterToFilter)

  //removeIdentityMaps is appropriate here because typed-indexing can introduce identity maps.
  def shareSubqueries[T](query: Exp[T]): Exp[T] =
    removeIdentityMaps(new SubquerySharing(subqueries).shareSubqueries(query))

  //Internally converts to flatMap normal form
  def handleFilters[T](exp: Exp[T]): Exp[T] =
    flatMapToMap(
      mergeFilters(
        hoistFilter( //Do this before merging filters!
          mapToFlatMap(exp))))

  //Call this whenever new MapNode nodes might be created, to simplify them if needed.
  //Requires map+flatMap normal form
  def handleNewMaps[T](exp: Exp[T]): Exp[T] =
    removeIdentityMaps( //Do this again, in case maps became identity maps after reassociation
      reassociateOps(betaDeltaReducer(
        mergeMaps(exp))))

  def basicInlining[T](exp: Exp[T]): Exp[T] = letTransformerUsedAtMostOnce(letTransformerTrivial(exp))

  //TODO: rewrite using function composition
  private def optimizeBase[T](exp: Exp[T]): Exp[T] =
  handleFilters(handleNewMaps(flatMapToMap(transformedFilterToFilter(betaDeltaReducer(mergeFilterWithMap(flatMapToMap(//simplifyForceView(filterToWithFilter(
    mergeFilters( //Merge filters again after indexing, since it introduces new filters.
      simplifyFilters(
        shareSubqueries(mapToFlatMap(
          handleNewMaps(
                cartProdToAntiJoin(
                  handleFilters(
                    optimizeCartProdToJoin(
                      removeRedundantOption(toTypeFilter(
                      //generalUnnesting, in practice, can produce the equivalent of let statements. Hence it makes sense to desugar them _after_ (at least the trivial ones).
                        flatMapToMap(sizeToEmpty(basicInlining(generalUnnesting(mapToFlatMap(
                          removeIdentityMaps(betaDeltaReducer(exp)))))))))))))))))))))))) //the call to reducer is to test.

  //The result of letTransformer is not understood by the index optimizer.
  //Therefore, we don't apply it at all on indexes, and we apply it to queries only after
  //subquery sharing.
  private def optimizeIdx[T](exp: Exp[T]): Exp[T] =
    flatMapToMap(optimizeBase(exp))

  //After letTransformer (an inliner), we can reduce redexes which arised; let's not do that, to avoid inlining
  // let definitions introduced by the user.
  def optimize[T](exp: Exp[T]): Exp[T] =
    flatMapToMap(letTransformer(betaDeltaReducer(optimizeBase(exp))))

  private val enableDebugLogStack = Stack(true)

  def pushEnableDebugLog(newVal: Boolean) {
    enableDebugLogStack push newVal
  }

  def popEnableDebugLog() {
    enableDebugLogStack.pop()
  }
  def isDebugLogEnabled = enableDebugLogStack.head
}
