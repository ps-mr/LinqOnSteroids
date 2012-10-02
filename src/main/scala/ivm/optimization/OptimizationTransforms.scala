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
  //reversed list of conds
  private def collectCondsReversed(exp: Exp[Boolean]): Seq[Exp[Boolean]] =
    exp match {
    //This works because And trees are left-associative, because of && precedence and because of reassociateBoolOps.
      case Sym(And(a, b)) => b +: collectCondsReversed(a)
      case _ => Seq(exp)
    }
  private def collectConds = collectCondsReversed _ andThen (_.reverse)

  //Split multiple filters anded together.
  val splitFilters: Exp[_] => Exp[_] = {
    case Sym(Filter(coll: Exp[Traversable[Any]], FunSym(f @ FuncExpBody(Sym(And(_, _)))))) =>
      collectConds(f.body).foldRight[Exp[Traversable[Any]]](coll)((cond, coll) => coll filter Fun.makefun(cond, f.x).f)
    case e => e
  }

  /*
  private def buildHoistedFilter[T, U, V](coll1: Exp[Traversable[T]], fmFun: Fun[T, Traversable[V]],
                                                    coll2: Exp[Traversable[U]],
                                                    filterFun: Fun[U, Boolean],
                                                    fmFun2: Fun[U, Traversable[V]]): Exp[Traversable[V]] = {
    //Let's show that the source types are correct, in that we can rebuild the original expression:
    import Util.assertType
    assertType[Exp[Traversable[V]]](coll1.flatMap(fmFun.f))
    assertType[Exp[Traversable[V]]](coll1.flatMap(Fun.makefun(coll2.filter(filterFun.f).flatMap(fmFun2.f), fmFun.x).f))
    val ret = coll1.withFilter(Fun.makefun(filterFun.body, fmFun.x).f) flatMap Fun.makefun(stripView(coll2) flatMap fmFun2.f, fmFun.x).f
    assertType[Exp[Traversable[V]]](ret)
    ret
  }

  //The body moves the filter up one level, but we want to do it multiple times, as many as needed.
  //However, we needn't apply this optimization in a fixpoint loop: the optimization is applied bottom up, which is
  //exactly what we need!
  //Scalac miscompiles this code if I write it the obvious way - without optimizations enabled!
  //coll1 >>= (\x. (filter coll2 (\y. filterFun y)) >>= fmFun2)
  val hoistFilter: Exp[_] => Exp[_] = {
    //This is wrong, since filterFun is the filter which is executed last on
    //coll2! We will move it and execute it before the other filters, which we decided not to do.
    //Alternatively, we could just take any filter and handle null checks specially.
    case FlatMap(coll1, fmFun@FuncExpBody(FlatMap(Filter(coll2, filterFun), fmFun2)))
      if !filterFun.body.isOrContains(filterFun.x) =>
      buildHoistedFilter(coll1, fmFun, coll2, filterFun, fmFun2)
    case e => e
  }
  */
  //The body moves the filter up one level, but we want to do it multiple times, as many as needed.
  private def buildHoistedFilter[T, U, V](coll1: Exp[Traversable[T]], fmFun: FunSym[T, Traversable[V]],
                                          coll2: Exp[Traversable[U]],
                                          filterFun: FunSym[U, Boolean], firstFilter: Exp[Boolean], otherFilters: Option[Exp[Boolean]],
                                          fmFun2: FunSym[U, Traversable[V]]): Exp[Traversable[V]] = {
    coll1.withFilter(Fun.makefun(firstFilter, fmFun.x).f).flatMap(
      Fun.makefun(otherFilters.fold(identity[Exp[Traversable[U]]] _)
                    ((filters: Exp[Boolean]) => (_ filter Fun.makefun(filters, filterFun.x).f)) apply
                  stripView(coll2) flatMap fmFun2.f, fmFun.x).f)
  }

    //This is to apply (recursively) after joining consecutive filters
  val hoistFilter: Exp[_] => Exp[_] = {
    case e @ Sym(FlatMap(coll1, fmFun @ FunSym(FuncExpBody(Sym(FlatMap(Sym(Filter(coll2: Exp[Traversable[u]], filterFun)), fmFun2)))))) =>
      val (firstFilter, otherFilters) = filterFun.body match {
        case Sym(And(firstFilter, otherFilters)) =>
          (firstFilter, Some(otherFilters))
        case body => (body, None)
      }
      /*coll1 withFilter Fun.makefun(firstFilter, fmFun.x).f flatMap
      Fun.makefun(otherFilters.fold(identity[Exp[Traversable[u]]] _)
                  ((filters: Exp[Boolean]) => (_ filter Fun.makefun(filters, filterFun.x))) apply stripView(coll2) flatMap fmFun2.f, fmFun.x).f*/

      //A recursive call is needed here to lift further filters.
      if (!firstFilter.isOrContains(filterFun.x))
        hoistFilter(buildHoistedFilter(coll1, fmFun, coll2, filterFun, firstFilter, otherFilters, fmFun2))
      else
        e
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
