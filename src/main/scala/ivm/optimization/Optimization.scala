package ivm
package optimization

import expressiontree._
import Lifting._
import collection.mutable.Stack
import scala.collection.mutable.Map
import performancetests.Benchmarking

object Optimization {
  val subqueries: Map[Exp[_], Any] = Map.empty

  def resetSubqueries() = subqueries.clear()
  def addIndex[T](_query: UnconvertedExp[Exp[T]], res: Option[T] = None) {
    val query = OptimizationUtil.stripViewUntyped(_query.v)
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

  def mergeFlatMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFlatMaps)

  def mergeViews[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeViews)

  def sizeToEmpty[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.sizeToEmpty)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = mergeViews(exp.transform(OptimizationTransforms.mergeFilters))

  def splitFilters[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.splitFilters)

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
  def betaDeltaReducer[T](exp: Exp[T]): Exp[T] = constantFolding(exp.transform(OptimizationTransforms.betaDeltaReducer))

  def existsRenester[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.existsRenester)

  def existsUnnester[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.existsUnnester)

  def resimplFilterIdentity[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.resimplFilterIdentity)

  def generalUnnesting[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.generalUnnesting)

  def simplifyForceView[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.simplifyForceView)

  def mergeFilterWithMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFilterWithMap)

  def transformedFilterToFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.transformedFilterToFilter)

  def filterToTransformedFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.filterToTransformedFilter)

  def constantFolding[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.constantFolding)

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
  private def optimizeBase[T](exp: Exp[T], idxLookup: Boolean = true): Exp[T] =
  handleFilters(handleNewMaps(flatMapToMap(transformedFilterToFilter(betaDeltaReducer(mergeFilterWithMap(flatMapToMap(//simplifyForceView(filterToWithFilter(
    mergeFilters( //Merge filters again after indexing, since it introduces new filters.
      simplifyFilters(
        (if (idxLookup) shareSubqueries[T] _ else identity[Exp[T]] _)(mapToFlatMap(
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
  def optimizeIdx[T](exp: Exp[T], idxLookup: Boolean = true): Exp[T] =
    checkIdempotent(exp, idxLookup, "optimizeIdx") {
      optimizeIdx(_, idxLookup = false)
    } {
      flatMapToMap(optimizeBase(exp, idxLookup))
    }

  //Check that optim(exp) == exp, but only if we are in debugging mode (flag Benchmarking.debugBench) and
  //if we are not recursively executing this check (flag doCheck)
  private def checkIdempotent[T](orig: Exp[T], doCheck: Boolean, name: String)(optim: Exp[T] => Exp[T])(exp: Exp[T]): Exp[T] = {
    if (Benchmarking.debugBench && doCheck) {
      val reOptim = optim(exp)
      if (exp != reOptim)
        Console.err.println("%s not idempotent on original query %s, optim. query %s, reoptimized query %s"
          format (name, orig, exp, reOptim))
    }

    exp
  }

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

  // Order in the end: first recognize operator, and only after that try fusion between different operators, since it
  // obscures structures to recognize.
  private def physicalOptimize[T](exp: Exp[T]): Exp[T] =
    (resimplFilterIdentity[T] _ compose existsRenester[T])(exp)
  //cartProdToAntiJoin(optimizeCartProdToJoin(
  private def newHandleFilters[T](exp: Exp[T]): Exp[T] =
    (mergeFilters[T] _ compose hoistFilter[T]
      compose splitFilters[T] compose simplifyConditions[T])(exp)

  private def filterFusion[T](exp: Exp[T]): Exp[T] =
    (newHandleFilters[T] _
      compose transformedFilterToFilter[T] compose betaDeltaReducer[T] compose basicInlining[T]
      compose mapToFlatMap[T] compose mergeFlatMaps[T] compose mergeMaps[T] compose flatMapToMap[T]
      compose betaDeltaReducer[T] compose filterToTransformedFilter[T])(exp)

  def newOptimize[T](exp: Exp[T]): Exp[T] = //No type annotation can be dropped in the body :-( - not without
  // downcasting exp to Exp[Nothing].
    (handleNewMaps[T] _ compose flatMapToMap[T] //compose ((x: Exp[T]) => /*transformedFilterToFilter*/(betaDeltaReducer(mergeFilterWithMap(flatMapToMap(x)))))
      compose filterFusion[T]
      compose physicalOptimize[T]
      compose betaDeltaReducer[T]
      compose newHandleFilters[T] //3s
      compose basicInlining[T] //5-7s
      compose existsUnnester[T] //6s
      compose generalUnnesting[T] //11s
      compose mapToFlatMap[T] //12-18s
      compose removeIdentityMaps[T] //40s
      compose betaDeltaReducer[T])(exp)
}
