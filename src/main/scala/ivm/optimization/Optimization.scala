package ivm
package optimization

import collection.mutable

import expressiontree._
import Lifting._
import tests.Debug

object Optimization {
  //Should the two normal forms (after flatMapToMap and after mapToFlatMap) be distinguished by different types?

  //Logging {{{
  private val enableDebugLogStack = mutable.Stack(true)

  def pushEnableDebugLog(newVal: Boolean) {
    enableDebugLogStack push newVal
  }

  def popEnableDebugLog() {
    enableDebugLogStack.pop()
  }
  def isDebugLogEnabled = enableDebugLogStack.head
  //}}}}

  //Indexing {{{
  val subqueries: mutable.Map[Exp[_], (Any, ClassTag[_], TypeTag[_])] = mutable.Map.empty
  val castedSubqueries = subqueries.asInstanceOf[mutable.Map[Exp[_], (Any, ClassTag[Any], TypeTag[Any])]]

  def resetSubqueries() = subqueries.clear()
  def addIndex[T: ClassTag: TypeTag](_query: UnconvertedExp[Exp[T]], res: Option[T] = None) {
    val query = OptimizationUtil.stripViewUntyped(_query.v)
    val optquery = optimizeIdx(query, idxLookup = true)
    optquery match {
      //XXX: This is an hack
      case Const(_) =>
      case _ =>
        val intQuery = res match {
          case Some(v) => v
          case None =>
            //Compile.toValue(optquery)
            optquery.interpret() //XXX: what if query is an incrementally maintained collection? We don't want to call interpret() again!
        }

        //Let us ensure that both the unoptimized and the optimized version of the query are recognized by the
        // optimizer. TODO: Reconsider again whether this is a good idea.
        subqueries += normalize(query) -> ((intQuery, classTag[T], typeTag[T]))
        subqueries += normalize(optquery) -> ((intQuery, classTag[T], typeTag[T]))
    }
  }

  def removeIndex[T](_query: UnconvertedExp[Exp[T]]) {
    val query = _query.v
    subqueries -= normalize(query)
    subqueries -= normalize(optimize(query))
  }

  val subquerySharer = new SubquerySharing(castedSubqueries)

  //removeIdentityMaps is appropriate here because typed-indexing can introduce identity maps.
  def shareSubqueries[T](query: Exp[T]): Exp[T] =
    removeIdentityMaps(subquerySharer.shareSubqueries(query))
  //}}}

  //Check that optim(exp) == exp, but only if we are in debugging mode (flag Benchmarking.debugBench) and
  //if we are not recursively executing this check (flag doCheck)
  private def checkIdempotent[T](orig: Exp[T], doCheck: Boolean, name: String)(optim: Exp[T] => Exp[T])(exp: Exp[T]): Exp[T] = {
    if (Debug.active && doCheck) {
      val reOptim = optim(exp)
      if (exp != reOptim)
        Console.err.println("%s not idempotent.\nOriginal query:\n>>>>> %s\nOptim. query:\n>>>>> %s\nReoptimized query:\n>>>>> %s"
          format (name, orig, exp, reOptim))
    }

    exp
  }

  /*
  TODO:
  sealed trait ExpNormalForm[T]
  case class FlatMapNF[T](e: Exp[T]) extends ExpNormalForm[T]
  case class MapNF[T](e: Exp[T]) extends ExpNormalForm[T]
  implicit def flatMapToExp: FlatMapNF[T] => Exp[T] = {
    case FlatMapNF(e) => e
  }
  implicit def mapToExp: MapNF[T] => Exp[T] = {
    case MapNF(e) => e
  }
  def mapToFlatMap[T]: Exp[T] => FlatMapNF[T]
  def flatMapToMap[T]: Exp[T] => MapNF[T]
  Maybe even tag the transformer and have Exp.transform produce the right type.
  */

  //Optimization entry points and major phases {{{

  //After letTransformer (which transforms flatMap-based lets into functions applications), we can reduce redexes which arised; let's not do that, to avoid inlining
  // let definitions introduced by the user.
  //The reasoning described above would imply that this optimizer is not idempotent, since we transform
  //stuff into App nodes which we beta-reduce only in a second call to the optimizer.
  //In fact, now beta reduction checks the inlining side conditions to guarantee idempotence. This however means that
  //we'll miss some opportunities for inlining and optimization, so maybe it should be reversed.
  //In that case, we need to call letTransformer(betaDeltaReducer(selectiveLetInliner(...))), which is however a bit wasteful.
  //So use betaDeltaReducer(letTransformer(...)) instead, at least for now.
  //
  //XXX: Questions, an year after: why do I have side conditions on let-to-beta-redex-transformations? Why do I use flatMap to implement let (because let would not work in for comprehensions? But it would! Why do I have side conditions on inlining both for flatmap-based lets and for beta-redexes? Is there anywhere a phase diagram showing what the heck is supposed to happen?)
  def optimize[T](exp: Exp[T], idxLookup: Boolean = true): Exp[T] =
    checkIdempotent(exp, idxLookup, "optimize") {
      optimize(_, idxLookup = false)
    } {
      //XXX: calling splitFilters instead of filterToWithFilter introduces a crazy bug in ITA_INEFFICIENT_TO_ARRAY
      filterToWithFilter(betterExists(flatMapToMap(betaDeltaReducer(betaDeltaReducer(letTransformer(optimizeBase(exp, idxLookup, forIdx = false)))))))
    }

  //The result of letTransformer is not understood by the index optimizer.
  //Therefore, we don't apply it at all on indexes, and we apply it to queries only after
  //subquery sharing.
  def optimizeIdx[T](exp: Exp[T], idxLookup: Boolean): Exp[T] =
    checkIdempotent(exp, idxLookup, "optimizeIdx") {
      optimizeIdx(_, idxLookup = false)
    } {
      flatMapToMap(optimizeBase(exp, idxLookup, forIdx = true))
    }

  private def maybeIndex[T](idxLookup: Boolean): Exp[T] => Exp[T] =
    if (idxLookup) shareSubqueries else identity

  //The result should be in flatMap normal form.
  private def optimizeBase[T](exp: Exp[T], idxLookup: Boolean, forIdx: Boolean): Exp[T] =
    postIndexing(forIdx, idxLookup, maybeIndex(idxLookup)(mapToFlatMap(preIndexing(exp))))

  //The result should be in flatMap normal form.
  private def postIndexing[T](forIdx: Boolean, idxLookup: Boolean, exp: Exp[T]): Exp[T] =
  //Why is mergeFilterWithMap&what comes after done only after indexing? I guess to avoid "hiding" filters inside map operations.
    handleFilters/*or just hoistFilter ?*/(maybeIndex(idxLookup)(mapToFlatMap(handleNewMaps(flatMapToMap(transformedFilterToFilter(betaDeltaReducer(mergeFilterWithMap(flatMapToMap(
      mergeFilters( //Merge filters again after indexing, since it introduces new filters.
        simplifyFilters(
          // XXX: tests if existsRenester works in this position, if it does not leave optimization
          // opportunities which require more aggressive optimizations, and in general for any negative side-effect
          // from doing this so late. Unfortunately, this must done after indexing.
          // It used instead to be called at the beginning of physicalOptimize.
          resimplFilterIdentity(if (forIdx) exp else existsRenester(exp)))))))))))))

  private[optimization] def preIndexing[T](exp: Exp[T]): Exp[T] = //No type annotation can be dropped in the body :-( - not without
  // downcasting exp to Exp[Nothing].
    (handleNewMaps[T] _ compose flatMapToMap[T] //compose ((x: Exp[T]) => /*transformedFilterToFilter*/(betaDeltaReducer(mergeFilterWithMap(flatMapToMap(x)))))
      compose filterFusion[T]
      compose physicalOptimize[T] //returns result of flatMapToMap
      compose betaDeltaReducer[T]
      compose newHandleFilters[T]
      compose selectiveLetInliner[T]
      compose existsUnnester[T]
      compose removeRedundantOption[T] compose toTypeFilter[T] compose sizeToEmpty[T]
      compose generalUnnesting[T]
      compose mapToFlatMap[T]
      compose removeIdentityMaps[T]
      compose betaDeltaReducer[T])(exp)

  //}}}

  //Pipeline building blocks {{{
  //Call this whenever new MapNode nodes might be created, to simplify them if needed.
  //Requires map+flatMap normal form
  def handleNewMaps[T](exp: Exp[T]): Exp[T] =
    removeIdentityMaps( //Do this again, in case maps became identity maps after reassociation
      reassociateOps(betaDeltaReducer(
        mergeMaps(exp))))

  /*
  private def preIndexingOld[T](exp: Exp[T]): Exp[T] =
      handleNewMaps(
        cartProdToAntiJoin(
          handleFilters(
            optimizeCartProdToJoin(
              flatMapToMap(removeRedundantOption(toTypeFilter(
                //generalUnnesting, in practice, can produce the equivalent of let statements. Hence it makes sense to desugar them _after_ (at least the trivial ones).
                sizeToEmpty(selectiveLetInliner(generalUnnesting(mapToFlatMap(
                  removeIdentityMaps(betaDeltaReducer(exp)))))))))))))
                  */

  // Order in the end: first recognize operator, and only after that try fusion between different operators, since it
  // obscures structures to recognize.
  // Requires result of mapToFlatMap, produces result of flatMapToMap
  private def physicalOptimize[T](exp: Exp[T]): Exp[T] =
    (cartProdToAntiJoin[T] _ compose optimizeCartProdToJoin[T] compose flatMapToMap[T])(exp)
  //cartProdToAntiJoin(optimizeCartProdToJoin(

  //Requires flatMap normal form
  private def newHandleFilters[T](exp: Exp[T]): Exp[T] =
    (handleFilters[T] _ compose simplifyConditions[T])(exp)
  //Shouldn't that be
  //(handleFilters[T] _ compose simplifyFilters[T])(exp)
  //? In general, TODO: replace simplifyConditions by simplifyFilters for extra robustness.


  //Requires flatMap normal form
  def handleFilters[T](exp: Exp[T]): Exp[T] =
    hoistFilter(mergeFilters(exp))

  //Accepts either normal form
  private def filterFusion[T](exp: Exp[T]): Exp[T] =
    (newHandleFilters[T] _
      //removeIdentityMaps can be triggered by transformedFilterToFilter
      compose mapToFlatMap[T] compose removeIdentityMaps[T] compose flatMapToMap[T]
      compose transformedFilterToFilter[T] compose betaDeltaReducer[T] compose selectiveLetInliner[T]
      compose mapToFlatMap[T] compose mergeFlatMaps[T] compose mergeMaps[T] compose flatMapToMap[T]
      compose betaDeltaReducer[T] compose ifSimplify[T] compose filterToTransformedFilter[T])(exp)

  //Boilerplate {{{
  def selectiveLetInliner[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.selectiveLetInliner)

  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToJoin)

  def cartProdToAntiJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToAntiJoin)

  def reassociateOps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.reassociateOps)

  def mergeMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeMaps)

  def mergeFlatMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFlatMaps)

  def sizeToEmpty[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.sizeToEmpty)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFilters)

  def splitFilters[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.splitFilters)

  def removeTrivialFilters[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeTrivialFilters)

  def simplifyConditions[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.simplifyConditions)

  def simplifyFilters[T](exp: Exp[T]): Exp[T] = removeTrivialFilters(simplifyConditions(exp))

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeIdentityMaps)

  def toTypeFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.toTypeFilter)

  //Requires flatMap normal form.
  def hoistFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.hoistFilter)

  def removeRedundantOption[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeRedundantOption)

  def mapToFlatMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mapToFlatMap)

  def flatMapToMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.flatMapToMap)

  def filterToWithFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.filterToWithFilter)

  def betterExists[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.betterExists)

  def letTransformer[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.letTransformer)

  //This should be called after any sort of inlining, including for instance map fusion.
  def betaDeltaReducer[T](exp: Exp[T]): Exp[T] = /*constantFolding*/simplifyConditions(exp.transform(OptimizationTransforms.betaDeltaReducer))

  def existsRenester[T](exp: Exp[T]): Exp[T] = dropUnusedBindings(exp.transform(OptimizationTransforms.existsRenester))

  def existsUnnester[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.existsUnnester)

  def resimplFilterIdentity[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.resimplFilterIdentity)

  private def dropUnusedBindings[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.dropUnusedBindings)

  def generalUnnesting[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.generalUnnesting)

  def simplifyForceView[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.simplifyForceView)

  def mergeFilterWithMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeFilterWithMap)

  def transformedFilterToFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.transformedFilterToFilter)

  def filterToTransformedFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.filterToTransformedFilter)

  def ifSimplify[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.ifSimplify)

  //def constantFolding[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.constantFolding)
  //}}}
}
