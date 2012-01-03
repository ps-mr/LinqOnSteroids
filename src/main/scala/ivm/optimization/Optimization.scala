package ivm
package optimization

import expressiontree._
import Lifting._
import collection.GenTraversableOnce

//Pattern-matchers for simplifying writing patterns
object FuncExpBody {
  def unapply[S, T](f: FuncExp[S, T]): Option[Exp[T]] = Some(f.body)
}

object FuncExpIdentity {
  def unapply[S, T](f: FuncExp[S, T]): Boolean = f.body == f.x
}

//Pattern match to connect two conditions
object & { def unapply[A](a: A) = Some(a, a) }

//XXX: make transform require a function of type Exp[T] to Exp[T]!
object OptimizationTransforms {
  private def buildJoin[T, S, TKey, TResult](fmColl: Exp[Traversable[T]],
                                                  wfColl: Exp[Traversable[S]],
                                                  lhs: Exp[TKey], rhs: Exp[TKey],
                                                  moFun: FuncExp[S, TResult], fmFun: FuncExp[T, GenTraversableOnce[TResult]],
                                                  wfFun: FuncExp[S, Boolean]): Exp[Traversable[TResult]] /*Join[T, S, TKey, TResult]*/ = {
    stripView(fmColl).join(
      stripView(wfColl))(
      FuncExp.makefun[T, TKey](lhs, fmFun.x).f,
      FuncExp.makefun[S, TKey](rhs, wfFun.x).f,
      FuncExp.makepairfun[T, S, TResult](
        moFun.body,
        fmFun.x,
        moFun.x).f)
  }

  /*
   * Optimizes expressions of the form:
   *   for (k <- l; k2 <- j if l(k) is r(k2)) yield mcf(k, k2)
   * that is:
   *   l.flatMap(k => j.withFilter(l(k) is r(_)).map(mcf(k, _)))
   * into:
   *   l.join(j, l, r, (p: Exp[(Int, Int)]) => mcf(p._1, p._2))
   * A problem appears if l or j is only FilterMonadic but not Traversable - and that won't be detected by the pattern
   * match.
   */
  val cartProdToJoin: Exp[_] => Exp[_] =
    e => e match {
      case FlatMap(fmColl: Exp[Traversable[_]],
        fmFun @ FuncExpBody(MapOp(Filter(filterColl: Exp[Traversable[_]], filterFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
        if !filterColl.isOrContains(fmFun.x)
      =>
        if (!(lhs.isOrContains(filterFun.x)) && !(rhs.isOrContains(fmFun.x)))
          buildJoin(fmColl, filterColl, lhs, rhs, moFun, fmFun, filterFun)
        else if (!(rhs.isOrContains(filterFun.x)) && !(lhs.isOrContains(fmFun.x)))
          buildJoin(fmColl, filterColl, rhs, lhs, moFun, fmFun, filterFun)
        else
          e
      case _ => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] =
    e => e match {
      case MapOp(col, FuncExpIdentity()) =>
        col
      case _ => e
    }

  //XXX: use normalization more often (e.g., whenever building a FuncExp, or whenever building a FuncExpInt?)
  private def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: FuncExp[T, U], g: FuncExp[U, V]) =
    coll.map(FuncExp.normalize(f.f andThen g.f, f.x))
    //coll.map(g.f andThen f.f) //Here the typechecker can reject this line.
  

  val mergeMaps: Exp[_] => Exp[_] =
    e => e match {
      case MapOp(MapOp(coll: Exp[Traversable[_]], f1), f2) =>
        //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
        //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
        mergeMaps(buildMergedMaps(coll, f1, f2))
      case _ => e
    }

  //XXX: This is a hack tailored to IVMPerformanceTests. OTOH, implementing the proper version seems just engineering
  val mergeOps: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(Plus(a, Const(b)), Const(c)) =>
        mergeOps(Plus(a, p.isNum.plus(b, c))(p.isNum))
      case _ => e
    }

  //Reassociation similarly to what is described in "Advanced Compiler Design and Implementation", page 337.
  //Note that we don't check whether computation is being done on floating-point numbers - for them, we should perform
  // no simplification.
  // - make sure that right children are always leaves and that constants are moved to the left
  // - perform constant folding on the left-hand child
  def buildSum[T](l: Exp[T], r: Exp[T])(implicit isNum: Numeric[T]): Exp[T] = {
    val e = l + r
    (l, r) match {
      case (Const(a), Const(b)) =>
        isNum.plus(a, b)
      case (Plus(Const(a), b), Const(c)) =>
        buildSum(isNum.plus(a, c), b)
      case (a, Plus(b, c)) =>
        //Note: only in this case are other simplifications possible at _this_ level.
        buildSum((a + b), c)
      case (Const(_), _) | (Plus(_, _), _) =>
        //Since the above matches failed and l is a Const, r is neither a Plus nor a Const node.
        e
      case (a, b@Const(_)) =>
        buildSum(b, a)
      case _ =>
        e
    }
  }
  
  val reassociateOps: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(l, r) =>
        buildSum(l, r)(p.isNum)
      case _ => e
    }

  val mergeFilters: Exp[_] => Exp[_] =
    e => e match {
      case Filter(Filter(col2: Exp[Traversable[_]], f2), f) =>
        mergeFilters(
          col2.withFilter{
            (x: Exp[_]) => And(f2(x), f(x))
          })
      case _ => e
    }

  //Recognize relational-algebra set operations; they can be executed more efficiently if one of the two members is indexed.
  //However, sets are already always indexed, so these optimizations will not have a huge impact by themselves.
  val setIntersection: Exp[_] => Exp[_] =
    e => e match {
      case Filter(col: Exp[Traversable[t]], predFun @ FuncExpBody(Contains(col2, x))) if (x == predFun.x) =>
        e //col.join(col2)(identity, identity, _._1) //Somewhat expensive implementation of intersection.
        //e //Intersect(col, col2)
      case _ => e
    }

  //We want to support anti-joins. Is this one? This is an anti-join where a set is involved and with identity selectors.
  val setDifference: Exp[_] => Exp[_] =
    e => e match {
      case Filter(col, predFun @ FuncExpBody(Not(Contains(col2, x)))) if (x == predFun.x) =>
        e //Diff(col, col2) //We cannot use Diff because col is not a Set - but we can build a more complex operator for this case.
      case _ => e
    }

  //Fuse multiple views
  val mergeViews: Exp[_] => Exp[_] =
    e => e match {
      case View(coll @ View(_)) =>
        coll
      case _ => e
    }
  
  val sizeToEmpty: Exp[_] => Exp[_] =
    e => e match {
      case Less(Const(0), Call1('size, _, coll: Exp[Traversable[t]])) =>
        coll.nonEmpty
      case LEq(Const(1), Call1('size, _, coll: Exp[Traversable[t]])) =>
        coll.nonEmpty
      case Not(Eq(Call1('size, _, coll: Exp[Traversable[t]]), Const(0))) =>
        coll.nonEmpty
      case Eq(Call1('size, _, coll: Exp[Traversable[t]]), Const(0)) =>
        coll.isEmpty
      case _ => e
    }

  val normalizer: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
      case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
      case _ => e
    }

  private[optimization] def stripView[T](coll: Exp[Traversable[T]]) =
    coll match {
      case View(coll2: Exp[Traversable[T]]) => coll2
      case _ => coll
    }
}

object Optimization {
  import scala.collection.mutable.Map

  val subqueries: Map[Exp[_], Any] = Map.empty

  def addSubQuery[T](query: Exp[T]) {
    val optquery = optimize(query)
    val intQuery = optquery.interpret() //XXX: what if query is an incrementally maintained collection? We don't want to call interpret() again!
    //Let us ensure that both the unoptimized and the optimized version of the query are recognized by the optimizer.
    // TODO: Reconsider again whether this is a good idea.
    subqueries += normalize(query) -> intQuery
    subqueries += normalize(optquery) -> intQuery
  }

  def removeSubQuery[T](query: Exp[T]) {
    subqueries -= normalize(query)
    subqueries -= normalize(optimize(query))
  }

  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToJoin)

  def optimize[T](exp: Exp[T]): Exp[T] = {
    shareSubqueries(
     mergeOps(
      mergeMaps(
       mergeFilters(
        optimizeCartProdToJoin(exp)))))
  }

  def mergeOps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeOps)

  def reassociateOps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.reassociateOps)

  def mergeMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeMaps)

  def mergeViews[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeViews)

  def sizeToEmpty[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.sizeToEmpty)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = mergeViews(exp.transform(OptimizationTransforms.mergeFilters))

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeIdentityMaps)

  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
      new SubquerySharing(subqueries).shareSubqueries(query)
  }

}
