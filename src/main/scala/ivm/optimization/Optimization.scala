package ivm
package optimization

import expressiontree._
import Lifting._
import collection.GenTraversableOnce
import Numeric.Implicits._

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

  private def buildAntiJoin[T, S, TKey](filteredColl: Exp[Traversable[T]],
                                                 forallColl: Exp[Traversable[S]],
                                                 lhs: Exp[TKey], rhs: Exp[TKey],
                                                 filterFun: FuncExp[T, Boolean],
                                                 forallFun: FuncExp[S, Boolean]): Exp[Traversable[T]] /*Join[T, S, TKey, TResult]*/ = {
    //XXX: in this version of the work, we should create a custom node, since our handling of redexes is not yet perfect -
    //we currently assume expression trees are already beta-reduced when comparing them. OTOH, performing beta-reduction
    //risks introducing non-termination inside optimization.

    //We must hoist the creation of the subcollection, so that we build the index only once. We use letExp to this end.
    letExp(FuncExp.makefun[T, TKey](lhs, filterFun.x)){
      subFun =>
        letExp((forallColl map FuncExp.makefun[S, TKey](rhs, forallFun.x).f).toSet){
          subColl =>
            stripView(filteredColl) withFilter {
              x =>
                !(subColl
                  contains
                  subFun(x))
            }}}

    //filteredColl withFilter (x => (forallColl forall forallFun))

    /*stripView(wfColl))(
  FuncExp.makefun[T, TKey](lhs, fmFun.x).f,
  FuncExp.makefun[S, TKey](rhs, wfFun.x).f,
  FuncExp.makepairfun[T, S, TResult](
    moFun.body,
    fmFun.x,
    moFun.x).f)*/
  }

  val cartProdToAntiJoin: Exp[_] => Exp[_] =
    e => e match {
      case Filter(filteredColl: Exp[Traversable[_]],
        filterFun @ FuncExpBody(Forall(forallColl, forallFun @ FuncExpBody(Not(Eq(lhs, rhs))))))
      //case FlatMap(fmColl: Exp[Traversable[_]],
        //fmFun @ FuncExpBody(MapOp(Filter(filterColl: Exp[Traversable[_]], filterFun @ FuncExpBody(Not(Eq(lhs, rhs)))), moFun)))
        if !forallColl.isOrContains(filterFun.x)
      =>
        if (!(rhs.isOrContains(filterFun.x)) && !(lhs.isOrContains(forallFun.x)))
          buildAntiJoin(filteredColl, forallColl, lhs, rhs, filterFun, forallFun)
        else if (!(lhs.isOrContains(filterFun.x)) && !(rhs.isOrContains(forallFun.x)))
          buildAntiJoin(filteredColl, forallColl, rhs, lhs, filterFun, forallFun)
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

  /*
  def removeIdentityMaps[T](e: Exp[T]): Exp[T] =
    e match {
      case MapOp(col: Exp[_ /*T*/], FuncExpIdentity()) =>
        col.asInstanceOf[Exp[T]]
      case _ => e
    }
    */

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

  // Reassociation similarly to what is described in "Advanced Compiler Design and Implementation", page 337-...,
  // Sec 12.3.1 and Fig 12.6. We refer to their reduction rules with their original codes, like R1, R2, ..., R9.
  // We don't apply distributivity, nor (yet) rules not involving just Plus (i.e., involving Minus or Times).
  // TODO: We should abstract this code to also apply on other commutative and associative operations.
  // Note that we don't check whether computation is being done on floating-point numbers - for them, we should perform
  // no simplification (Sec. 12.3.2).
  //
  // We make sure that right children are never Plus expressions and that constants are moved to the left (using rules R2 and R7).
  // All `Const` nodes are then constant-folded together (using rules R1 and R9).
  //
  // Note: keep in mind that the transformation is applied bottom-up; moreover, whenever a new expression is built, new sums
  // are created by recursive invocation of buildSum, except in the default case.
  // Note 2: in their picture, "t_x" represents an arbitrary node, even in rule R7.
  // Note 3: rule R2 and R7 together seem not to form a terminating rewrite system; note however that
  // rule R7 does not just swap children but performs a tree rotation.
  // Note 4: we omit rules on subtractions (R5, R6, etc.) because NumericOps.- just always represent subtraction through
  // addition (XXX which is a pessimization if no constants are involved)
  def buildSum[T: Numeric](l: Exp[T], r: Exp[T]): Exp[T] = {
    r match {
      case Const(rV) => l match {
        case Const(a) => //R1
          a + rV
        case Plus(Const(a), b) => //R9 - must be before R2!
          buildSum(a + rV, b)
        case _ => //R2 - must be after R1!
          buildSum(r, l)
      }
      case Plus(rl, rr) => //R7
        buildSum(buildSum(l, rl), rr)
      case _ =>
        l + r
    }
    /*
     * Rules R1 and R9 reduce the tree size by one, thus it is easy to prove that their application causes
     * well-founded recursion. Rules R2 and R7 are instead more problematic.
     * Why does the above recursive invocation in R2 terminate? This is obvious by case analysis unless l is a Plus
     * node. Otherwise, we need to prove that we either terminate recursion or that we reach either of rule R1 or
     * R9, reducing the input size. We must do the proof by induction, assuming that all calls to buildSum with
     * total input size smaller than the current one terminate.
     *
     * Let us assume (l, r) matches (Plus(l1, l2), Const(rV)); buildSum(b, a) will match
     * (r, l) against (Const(rV), Plus(l1, l2)) (R7), and rewrite it to buildSum(buildSum(Const(rV), l1), l2).
     * If l2 is not a Const node, it will not match again this case (rule R7) without further reduction.
     * If l2 is a Const node and the inner buildSum(Const(rV), l1) just returns Plus(Const(rV), l1), rule R9 applies
     * and reduces the input size.
     * If l2 is a Const node and the inner buildSum(Const(rV), l1) returns something else, then it must terminate
     * by the inductive hypothesis.
     */
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
      case Less(Const(0), Size(coll)) =>
        coll.nonEmpty
      case LEq(Const(1), Size(coll)) =>
        coll.nonEmpty
      case Not(Eq(Size(coll), Const(0))) =>
        coll.nonEmpty
      case Eq(Size(coll), Const(0)) =>
        coll.isEmpty
      case _ => e
    }

  private def buildTypeFilter[S, T, U](coll: Exp[Traversable[T]], cS: ClassManifest[S], f: FuncExp[S, Traversable[U]]): Exp[Traversable[U]] =
    //coll.typeFilter(cS).map(f.f)
    coll.typeFilter(cS).flatMap(f.f)

  private def tryBuildTypeFilter[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FuncExp[T, TraversableOnce[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val X = fmFun.x
    //Correct safety condition for this optimization: The variable of fmFun must appear always wrapped in the same
    //IfInstanceOf node (with the same type manifest...)
    val containingX = fmFun.body.findTotFun(_.children.contains(X))
    containingX.head match {
      case instanceOfNode@IfInstanceOf(X) if containingX.forall(_ == instanceOfNode) =>
        //Aargh! Do something about this mess, to allow expressing it in a nicer way.
        val v = FuncExp.gensym()
        val transformed = fmFun.body.substSubTerm(instanceOfNode, Let(v))
        //Note: on the result we would really like to drop all the 'Option'-ness, but that's a separate step.
        buildTypeFilter(coll, instanceOfNode.cS, FuncExp.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v))
      case _ =>
        e
    }
  }

  /*
  fmFun match {
    /*case FuncExpBody(Call1(`optionToIterableId`, _, Call2(`optionMapId`, _, instanceOf@IfInstanceOf(x), f: FuncExp[Any, _]))) =>
  buildTypeFilter(coll, instanceOf.cS, f)*/
   */

  val toTypeFilter: Exp[_] => Exp[_] = {
    e => e match {
      case FlatMap(coll: Exp[Traversable[_]], fmFun: FuncExp[t, u]) =>
        tryBuildTypeFilter(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
      case _ => e
    }
  }

  private def buildHoistedFilterForMap[T, U, V](coll1: Exp[Traversable[T]], fmFun: FuncExp[T, TraversableOnce[V]],
                                coll2: Exp[Traversable[U]],
                                filterFun: FuncExp[U, Boolean],
                                mapFun: FuncExp[U, V]): Exp[Traversable[V]] = {
    //Let's show that the source types are correct, in that we can rebuild the original expression:
    import Util.assertType
    assertType[Exp[Traversable[V]]](coll1.flatMap(fmFun.f))
    assertType[Exp[Traversable[V]]](coll1.flatMap(FuncExp.makefun(coll2.filter(filterFun.f).map(mapFun.f), fmFun.x).f))
    val ret: Exp[Traversable[V]] = coll1.withFilter(FuncExp.makefun(filterFun.body, fmFun.x).f) flatMap FuncExp.makefun(stripView(coll2) map mapFun.f, fmFun.x).f
    ret
  }

  private def buildHoistedFilterForFlatMap[T, U, V](coll1: Exp[Traversable[T]], fmFun: FuncExp[T, TraversableOnce[V]],
                                                coll2: Exp[Traversable[U]],
                                                filterFun: FuncExp[U, Boolean],
                                                fmFun2: FuncExp[U, TraversableOnce[V]]): Exp[Traversable[V]] = {
    //Let's show that the source types are correct, in that we can rebuild the original expression:
    import Util.assertType
    assertType[Exp[Traversable[V]]](coll1.flatMap(fmFun.f))
    assertType[Exp[Traversable[V]]](coll1.flatMap(FuncExp.makefun(coll2.filter(filterFun.f).flatMap(fmFun2.f), fmFun.x).f))
    val ret: Exp[Traversable[V]] = coll1.withFilter(FuncExp.makefun(filterFun.body, fmFun.x).f) flatMap FuncExp.makefun(stripView(coll2) flatMap fmFun2.f, fmFun.x).f
    ret
  }

  //TODO: apply this optimization in a fixpoint loop, to move the filter as much as needed.
  //Scalac miscompiles this code if I write it the obvious way - without optimizations enabled!
  val hoistFilter: Exp[_] => Exp[_] =
    e => {
      val e1 = e match {
        case FlatMap(coll1: Exp[Traversable[_]], fmFun @ FuncExpBody(MapOp(Filter(coll2: Exp[Traversable[_]], filterFun), mapFun)))
          if !filterFun.body.isOrContains(filterFun.x) =>
          buildHoistedFilterForMap(coll1, fmFun, coll2, filterFun, mapFun)
        //coll1.filter(FuncExp.makefun(filterFun.body, fmFun.x).f) flatMap FuncExp.makefun(coll map mapFun.f, fmFun.x).f
          /*
        case FlatMap(coll1: Exp[Traversable[_]], fmFun @ FuncExpBody(FlatMap(Filter(coll2: Exp[Traversable[_]], filterFun), fmFun2)))
          if !filterFun.body.isOrContains(filterFun.x) =>
          buildHoistedFilterForFlatMap(coll1, fmFun, coll2, filterFun, fmFun2)
          */
        case _ => e
      }
      e1 match {
        case FlatMap(coll1: Exp[Traversable[_]], fmFun @ FuncExpBody(FlatMap(Filter(coll2: Exp[Traversable[_]], filterFun), fmFun2)))
          if !filterFun.body.isOrContains(filterFun.x) =>
          buildHoistedFilterForFlatMap(coll1, fmFun, coll2, filterFun, fmFun2)
        case _ => e1
      }
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

  def cartProdToAntiJoin[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.cartProdToAntiJoin)

  def optimize[T](exp: Exp[T]): Exp[T] = {
    shareSubqueries(removeIdentityMaps(
      reassociateOps(
        mergeMaps(
          mergeFilters(
            optimizeCartProdToJoin(exp))))))
  }

  def reassociateOps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.reassociateOps)

  def mergeMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeMaps)

  def mergeViews[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mergeViews)

  def sizeToEmpty[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.sizeToEmpty)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = mergeViews(exp.transform(OptimizationTransforms.mergeFilters))

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeIdentityMaps)

  def toTypeFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.toTypeFilter)

  def hoistFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.hoistFilter)

  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
      new SubquerySharing(subqueries).shareSubqueries(query)
  }
}
