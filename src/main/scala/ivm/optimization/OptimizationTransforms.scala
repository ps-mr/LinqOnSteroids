package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._
import collection.generic.CanBuildFrom
import collection.TraversableLike

/*
 * Note: it is crucial that optimization do not build expression nodes through their constructors, as they are not part
 * of the interface exported to the optimization module. The rationale is that different semantics, for instance
 * incremental view maintenance, require different nodes to be used for the same operation. The only constraint which is part of the
 * interface is that the same extractor work for those nodes as well; in practice, different semantics refine expression
 * nodes by subclassing them.
 *
 * This is IMHO relevant for the paper.
 */

//XXX: make transform require a function of type Exp[T] to Exp[T]!
object OptimizationTransforms extends NumericOptimTransforms with SimplificationsOptimTransforms with
  Inlining with FoldPhysicalOperators with Unnesting with Fusion with TypeFilterOptim
{
  //list of conds
  private def collectConds(exp: Exp[Boolean]): Seq[Exp[Boolean]] =
    exp match {
      //This works because And trees are right-associative (because of reassociateBoolOps).
      case Sym(And(a, b)) => a +: collectConds(b)
      case _ => Seq(exp)
    }
  //private def collectConds = collectCondsReversed _ andThen (_.reverse)

  //Split multiple filters anded together.
  val splitFilters: Exp[_] => Exp[_] = {
    case Sym(Filter(coll: Exp[Traversable[Any]], FunSym(f @ FuncExpBody(Sym(And(_, _)))))) =>
      collectConds(f.body).foldRight[Exp[Traversable[Any]]](coll)((cond, coll) => coll filter Fun.makefun(cond, f.x).f)
    case e => e
  }

  private def buildHoistedFilter[T, U, V](coll1: Exp[Traversable[T]], fmFun: FunSym[T, Traversable[V]],
                                          coll2: Exp[Traversable[U]],
                                          filterFun: FunSym[U, Boolean], filtersToHoist: Seq[Exp[Boolean]], otherFilters: Seq[Exp[Boolean]],
                                          fmFun2: FunSym[U, Traversable[V]]): Option[Exp[Traversable[V]]] = {
    if (filtersToHoist.nonEmpty) {
      val hoistedPred = Fun.makefun(filtersToHoist reduceRight (_ && _), fmFun.x)
      val coll2WithOtherFilters =
        ExpTransformer(removeTrivialFilters) {
          stripView(coll2) filter Fun.makefun((otherFilters foldRight asExp(true)) (_ && _), filterFun.x).f
        }
      Some(coll1 withFilter hoistedPred.f flatMap Fun.makefun(coll2WithOtherFilters flatMap fmFun2.f, fmFun.x).f)
    } else
      None
  }

  //Given
  //coll1 flatMap (x => coll2 filter (y => filterFun) flatMap fmFun2)
  //split filterFun: Exp[Boolean] into a part which does not refer to y, filtersToHoist, and another part, otherFilters.
  //Then produce:
  //coll1 filter (x => filtersToHoist) flatMap (x => coll2 filter (y => otherFilters) flatMap fmFun2)
  //
  //This is to apply after fusing consecutive filters.
  //The body moves the filter up one level, but we want to do it multiple times, as many as needed.
  //However, we needn't apply this optimization in a fixpoint loop: the optimization is applied bottom up, which is
  //exactly what we need!
  val hoistFilter: Exp[_] => Exp[_] = {
    case e @ Sym(FlatMap(coll1, fmFun @ FunSym(FuncExpBody(Sym(FlatMap(Sym(Filter(coll2, filterFun)), fmFun2)))))) =>
      val allFilters = collectConds(filterFun.body)
      val (otherFilters, filtersToHoist) = allFilters partition (_ isOrContains filterFun.x)
      buildHoistedFilter(coll1, fmFun, coll2, filterFun, filtersToHoist, otherFilters, fmFun2) getOrElse e
    case e => e
  }

  private def
  buildMapToFlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]]
  (c: Exp[Repr], f: FunSym[T, U], cbf: CanBuildFrom[Repr, U, That]): Exp[That] =
    (c flatMap Fun.makefun(Seq(f.body), f.x).f)(cbf)

  val mapToFlatMap: Exp[_] => Exp[_] = {
    case Sym(m @ MapNode(c, f)) =>
      buildMapToFlatMap(c, f, m.c)
    case e => e
  }

  private def buildFlatMapToMap[T, U](c: Exp[Traversable[T]], body: Exp[U], f: Fun[T, Traversable[U]]): Exp[Traversable[U]] =
    c map Fun.makefun(body, f.x).f

  val flatMapToMap: Exp[_] => Exp[_] = {
    case Sym(FlatMap(c, FunSym(f@FuncExpBody(Sym(ExpSeq(Seq(body))))))) =>
      buildFlatMapToMap(c, body, f)
    case e => e
  }

  //Converts Filter nodes into WithFilter where possible without changing the type.
  //What I did before was introducing View and Force nodes; well, it did not speed up queries.
  // Add view and force around Filter to imitate withFilter.
  // Transformation rule:
  // coll.filter(f).*map(g) -> coll.view.filter(f).*map(g).force
  // where *map stands for map or flatMap (the same on both sides).
  //
  //Expects a query in map normal form.
  val filterToWithFilter: Exp[_] => Exp[_] = {
    case Sym(FlatMap(Sym(Filter(coll, p)), f)) =>
      fmap(fmap(coll, p, 'TraversableLike)('withFilter, _ withFilter _), f, 'FilterMonadic)('flatMap, _ flatMap _)
      //WithFilter(coll, p) flatMap f //wrong return type, still view-based...
    case Sym(MapNode(Sym(Filter(coll, p)), f)) => //Since flatMap after this query will not be transformed into maps, we need to have
      //a separate case.
      fmap(fmap(coll, p, 'TraversableLike)('withFilter, _ withFilter _), f, 'FilterMonadic)('map, _ map _)
    case e => e
  }

  val betterExists: Exp[_] => Exp[_] = {
    case Sym(IsEmpty(Sym(Filter(coll, pred)))) => !asExp(Exists(coll, pred))
//    //XXX We have this special case, instead of relying on simplification of
//    //Not(Not(x)) => x, just because we run this optimization at the very end
//    //of the pipeline.
//    case Not(IsEmpty(Filter(coll, pred))) => Exists(coll, pred)
    //That case won't trigger since the traversal is bottom up. So instead:
    case Sym(Not(Sym(Not(x)))) => x
    case e => e
  }

  val normalizer: Exp[_] => Exp[_] = {
    case Sym(p@Plus(x, y)) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
    case Sym(Eq(x, y)) => Eq(Exp.min(x, y), Exp.max(x, y))
    case e => e
  }
}
