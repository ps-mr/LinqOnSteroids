package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait FoldPhysicalOperators {
  private def buildJoin[T, S, TKey, TResult](fmColl: Exp[Traversable[T]],
                                             wfColl: Exp[Traversable[S]],
                                             lhs: Exp[TKey], rhs: Exp[TKey],
                                             moFun: Fun[S, TResult], fmFun: Fun[T, Traversable[TResult]],
                                             wfFun: Fun[S, Boolean], origExp: Exp[Traversable[TResult]]): Exp[Traversable[TResult]] /*Join[T, S, TKey, TResult]*/ = {
    if (lhs.isOrContains(fmFun.x) && rhs.isOrContains(wfFun.x)) {
      stripView(fmColl).join(
        stripView(wfColl))(
        Fun.makefun[T, TKey](lhs, fmFun.x).f,
        Fun.makefun[S, TKey](rhs, wfFun.x).f,
        Fun.makepairfun[T, S, TResult](
          moFun.body,
          fmFun.x,
          moFun.x).f)
    } else {
      //The two key extractors are executed for each element of the respective collection; if either of them is constant,
      //a join becomes just a loop with a higher constant factor - hence skip the optimization.
      origExp
    }
  }

  /*
   * Optimizes expressions of the form:
   *   for (k <- l; k2 <- j if l(k) is r(k2)) yield mcf(k, k2)
   * that is:
   *   l.flatMap(k => j.withFilter(l(k) is r(_)).map(mcf(k, _)))
   * into:
   *   l.join(j, l, r, (p: Exp[(Int, Int)]) => mcf(p._1, p._2))
   */
  //XXX: rewrite to expect only flatMap nodes, i.e. after mapToFlatMap, and see what happens.
  val cartProdToJoin: Exp[_] => Exp[_] = {
    case e@FlatMap(fmColl,
    fmFun@FuncExpBody(MapNode(Filter(filterColl, filterFun@FuncExpBody(Eq(lhs, rhs))), moFun)))
      if !filterColl.isOrContains(fmFun.x)
    =>
      if (!(lhs.isOrContains(filterFun.x)) && !(rhs.isOrContains(fmFun.x)))
        buildJoin(fmColl, filterColl, lhs, rhs, moFun, fmFun, filterFun, e)
      else if (!(rhs.isOrContains(filterFun.x)) && !(lhs.isOrContains(fmFun.x)))
        buildJoin(fmColl, filterColl, rhs, lhs, moFun, fmFun, filterFun, e)
      else
        e
    case e => e
  }

  private def buildAntiJoin[T, S, TKey](filteredColl: Exp[Traversable[T]],
                                        forallColl: Exp[Traversable[S]],
                                        lhs: Exp[TKey], rhs: Exp[TKey],
                                        filterFun: Fun[T, Boolean],
                                        forallFun: Fun[S, Boolean]): Exp[Traversable[T]] /*Join[T, S, TKey, TResult]*/ = {
    //XXX: in this version of the work, we should create a custom node, since our handling of redexes is not yet perfect -
    //we currently assume expression trees are already beta-reduced when _comparing_ them and looking for common subexpressions.
    //OTOH, performing beta-reduction risks introducing non-termination inside optimization.

    //We must hoist the creation of the subcollection, so that we build the index only once. We use letExp to this end.
    val lhsFun = Fun.makefun[T, TKey](lhs, filterFun.x)
    // lhsFun is used only in one location in a loop; thanks to normalization-by-evaluation, we can inline it while being
    // sure that term manipulation is only done at optimization time.
    letExp((forallColl map Fun.makefun[S, TKey](rhs, forallFun.x).f).toSet) {
      subColl =>
        stripView(filteredColl) withFilter {
          x =>
            !(subColl contains lhsFun(x))
        }
    }
  }

  val cartProdToAntiJoin: Exp[_] => Exp[_] = {
    case e@Filter(filteredColl,
    //XXX: doesn't work, add unit test - Forall nodes are just not generated.
    filterFun@FuncExpBody(Forall(forallColl, forallFun@FuncExpBody(Not(Eq(lhs, rhs))))))
      //case FlatMap(fmColl,
      //fmFun @ FuncExpBody(MapNode(Filter(filterColl, filterFun @ FuncExpBody(Not(Eq(lhs, rhs)))), moFun)))
      if !forallColl.isOrContains(filterFun.x)
    =>
      if (!(rhs.isOrContains(filterFun.x)) && !(lhs.isOrContains(forallFun.x)))
        buildAntiJoin(filteredColl, forallColl, lhs, rhs, filterFun, forallFun)
      else if (!(lhs.isOrContains(filterFun.x)) && !(rhs.isOrContains(forallFun.x)))
        buildAntiJoin(filteredColl, forallColl, rhs, lhs, filterFun, forallFun)
      else
        e
    case e => e
  }

  //Recognize relational-algebra set operations; they can be executed more efficiently if one of the two members is indexed.
  //However, sets are already always indexed, so these optimizations will not have a huge impact by themselves.
  val setIntersection: Exp[_] => Exp[_] = {
    case e@Filter(col, predFun@FuncExpBody(Contains(col2, x))) if (x == predFun.x) =>
      //XXX transformation not implemented
      e //col.join(col2)(identity, identity, _._1) //Somewhat expensive implementation of intersection.
    //e //Intersect(col, col2)
    case e => e
  }

  //We want to support anti-joins. Is this one? This is an anti-join where a set is involved and with identity selectors.
  val setDifference: Exp[_] => Exp[_] = {
    case e@Filter(col, predFun@FuncExpBody(Not(Contains(col2, x)))) if (x == predFun.x) =>
      //XXX transformation not implemented
      e //Diff(col, col2) //We cannot use Diff because col is not a Set - but we can build a more complex operator for this case.
    case e => e
  }

  val sizeToEmpty: Exp[_] => Exp[_] = {
    case Less(Const(0), Size(coll)) =>
      coll.nonEmpty
    case LEq(Const(1), Size(coll)) =>
      coll.nonEmpty
    case Not(Eq(Size(coll), Const(0))) =>
      coll.nonEmpty
    case Eq(Size(coll), Const(0)) =>
      coll.isEmpty
    case e => e
  }
}
