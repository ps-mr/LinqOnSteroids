package ivm
package optimization

import expressiontree._
import Lifting._
import collection.GenTraversableOnce
import Numeric.Implicits._
import annotation.tailrec

//Pattern-matchers for simplifying writing patterns
object FuncExpBody {
  def unapply[S, T](f: FuncExp[S, T]): Option[Exp[T]] = Some(f.body)
}

object FuncExpBodyUntyped {
  def unapply(f: FuncExp[_, _]): Option[Exp[_]] = Some(f.body)
}

object FuncExpIdentity {
  def unapply[S, T](f: FuncExp[S, T]): Boolean = f.body == f.x
}

//Pattern match to connect two conditions
object & { def unapply[A](a: A) = Some(a, a) }

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
   */
  //XXX: rewrite to expect only flatMap nodes, i.e. after mapToFlatMap, and see what happens.
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
    //we currently assume expression trees are already beta-reduced when _comparing_ them and looking for common subexpressions.
    //OTOH, performing beta-reduction risks introducing non-termination inside optimization.

    //We must hoist the creation of the subcollection, so that we build the index only once. We use letExp to this end.
    val lhsFun = FuncExp.makefun[T, TKey](lhs, filterFun.x)
    // lhsFun is used only in one location in a loop; thanks to normalization-by-evaluation, we can inline it while being
    // sure that term manipulation is only done at optimization time.
    letExp((forallColl map FuncExp.makefun[S, TKey](rhs, forallFun.x).f).toSet){
      subColl =>
        stripView(filteredColl) withFilter {
          x =>
            !(subColl contains lhsFun(x))
        }}
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

  val removeTrivialFilters: Exp[_] => Exp[_] =
    e => e match {
      case Filter(coll, FuncExpBody(Const(true))) =>
        stripViewUntyped(coll)
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

  //Same problem also when fusing map and flatMap (which we don't do yet).
  private def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: FuncExp[T, U], g: FuncExp[U, V]) =
    coll.map(x => letExp(f(x))(g))
    //coll.map(f.f andThen g.f) //Old implementation, equivalent to inlining f(x) in the body of g. This might duplicate work!
    //coll.map(g.f andThen f.f) //Here the typechecker can reject this line.

  //Now we'd like a non-work-duplicating inliner. We'd need a linear type system as in GHC's inliner.

  val mergeMaps: Exp[_] => Exp[_] =
    e => e match {
      case MapOp(MapOp(coll: Exp[Traversable[_]], f1), f2) =>
        //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
        //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
        mergeMaps(buildMergedMaps(coll, f1, f2))
      case _ => e
    }

  //Express in the type system that transformations need to preserve typing:
  trait Transformer {
    def apply[T](e: Exp[T]): Exp[T]
  }
  val mergeMaps2: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case MapOp(MapOp(coll: Exp[Traversable[t]], f1), f2) =>
        mergeMaps2(buildMergedMaps(coll, f1, f2)).
          //Note the need for this cast.
          asInstanceOf[Exp[T]]
      case _ => e
    }
  }

  // Reassociation similarly to what is described in "Advanced Compiler Design and Implementation", page 337-...,
  // Sec 12.3.1 and Fig 12.6. We refer to their reduction rules with their original codes, like R1, R2, ..., R9.
  // We don't apply distributivity, nor (yet) rules not involving just Plus (i.e., involving Minus or Times).
  // XXX: We should abstract this code to also apply on other commutative and associative operations. However, there's only * at the moment.
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

  //Copy-n-paste of buildSum. We don't use the distributive rule currently.
  def buildProd[T: Numeric](l: Exp[T], r: Exp[T]): Exp[T] = {
    r match {
      case Const(rV) => l match {
        case Const(a) => //R1
          a * rV
        case Plus(Const(a), b) => //R9 - must be before R2!
          buildProd(a * rV, b)
        case _ => //R2 - must be after R1!
          buildProd(r, l)
      }
      case Times(rl, rr) => //R7
        buildProd(buildProd(l, rl), rr)
      case _ =>
        l * r
    }
  }

  val reassociateOps: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(l, r) =>
        buildSum(l, r)(p.isNum)
      case t@Times(l, r) =>
        buildProd(l, r)(t.isNum)
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
        //XXX transformation not implemented
        e //col.join(col2)(identity, identity, _._1) //Somewhat expensive implementation of intersection.
        //e //Intersect(col, col2)
      case _ => e
    }

  //We want to support anti-joins. Is this one? This is an anti-join where a set is involved and with identity selectors.
  val setDifference: Exp[_] => Exp[_] =
    e => e match {
      case Filter(col, predFun @ FuncExpBody(Not(Contains(col2, x)))) if (x == predFun.x) =>
        //XXX transformation not implemented
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

  private def buildTypeFilter[S, T, U](coll: Exp[Traversable[T]], classS: Class[S], f: FuncExp[S, Traversable[U]], origFmFun: FuncExp[T, TraversableOnce[U]]): Exp[Traversable[U]] = {
    val res = coll.typeFilterClass(classS).flatMap(f.f)
    //Check that the transformed expression has overall the same type as the original one:
    Util.checkSameTypeAndRet(coll flatMap origFmFun)(res)
  }

  private def tryBuildTypeFilter[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FuncExp[T, TraversableOnce[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val X = fmFun.x
    //Correct safety condition for this optimization: The variable of fmFun must appear always wrapped in the same
    //IfInstanceOf node (with the same type manifest...)
    val containingX = fmFun.body.findTotFun(_.children.contains(X))
    containingX.head match {
      case instanceOfNode@IfInstanceOf(X, _) if containingX.forall(_ == instanceOfNode) =>
        //Aargh! Do something about this mess, to allow expressing it in a nicer way.
        val v = FuncExp.gensym()
        val transformed = fmFun.body.substSubTerm(instanceOfNode, Some(v))
        //Note: on the result we would really like to drop all the 'Option'-ness, but that's a separate step.
        buildTypeFilter(coll, instanceOfNode.classS, FuncExp.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)
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

  //removeRedundantOption is supposed to eliminate redundant lets from code like:
  //for (i <- base.typeFilter[Int]; j <- Some(i) if j % 2 ==# 0) yield j
  //which is for instance produced by toTypeFilter.
  //This optimization does not extend to normal bindings as used in FindBugsAnalyses. There we need to produce the usual
  //desugaring of Let - i.e. to use letExp.
  private def tryRemoveRedundantOption[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FuncExp[T, TraversableOnce[U]],
                                       insideConv: Exp[Option[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    import OptionOps._

    // The safety condition for this optimization is two-fold:
    // 1. The variable of fmFun must appear always wrapped in the same
    //    Some node
    // 2. Only supported Option operations must appear.
    
    //Check safety condition, part 2.
    @tailrec
    def isSupported(insideConv: Exp[_], LetNode: Exp[_]): Boolean =
      insideConv match {
        case Call2(OptionMapId, _, subColl, _) => isSupported(subColl, LetNode)
        case Call2(OptionFilterId, _, subColl, _) => isSupported(subColl, LetNode)
        case Call2(OptionFlatMapId, _, subColl, _) => isSupported(subColl, LetNode)
        case LetNode => true
        case _ => false
      }

    val X = fmFun.x
    val containingX = insideConv.findTotFun(_.children.contains(X))
    containingX.head match {
      case letNode@ExpOption(Some(X)) if containingX.forall(_ == letNode) && isSupported(insideConv, letNode) =>
        //Aargh! Do something about this mess, to allow expressing it in a nicer way.
        //val v = FuncExp.gensym()
        //val transformed = insideConv.substSubTerm(letNode, v).asInstanceOf[Exp[U]] //Note that the type, in fact, should change somehow!
        val transformed = insideConv.substSubTerm(letNode, coll).asInstanceOf[Exp[Traversable[U]]] //Note that the type, in fact, should change somehow!
        val transformed2 = transformed transform (e2 => e2 match {
          //The type annotations on subColl reflect on purpose types after transformation
          case Call2(OptionMapId, _, subColl: Exp[Traversable[t]], f: FuncExp[_, u]) =>
            subColl map f.asInstanceOf[FuncExp[t, u]].f
          case Call2(OptionFilterId, _, subColl: Exp[Traversable[t]], f: FuncExp[_, _]) =>
            //Let's just guess that the query was written with withFilter instead of filter, and use withFilter in the
            //transformed query
            subColl withFilter f.asInstanceOf[FuncExp[t, Boolean]].f
          case Call2(OptionFlatMapId, _, subColl: Exp[Traversable[t]], f: FuncExp[_, TraversableOnce[u]]) =>
            subColl flatMap f.asInstanceOf[FuncExp[t, TraversableOnce[u]]].f
          case _ => e2
        })
        //coll.map(FuncExp.makefun(transformed2, v).f)
        transformed2
      case _ =>
        e
    }
  }

  val removeRedundantOption: Exp[_] => Exp[_] = {
    import OptionOps._
    e => e match {
      case FlatMap(coll: Exp[Traversable[t]], (fmFun: FuncExp[_, Traversable[u]]) & FuncExpBody(Call1(OptionToIterableId, _, insideConv: Exp[Option[_]]))) =>
        tryRemoveRedundantOption(coll, fmFun, insideConv.asInstanceOf[Exp[Option[u]]], e.asInstanceOf[Exp[Traversable[u]]])
      /*case FlatMap(coll, fmFun @ FuncExpBody(Call1(OptionToIterableId, _, Call2(OptionMapId, _, subColl, f: FuncExp[Any, _])))) =>
        e
      case FlatMap(coll, fmFun @ FuncExpBody(Call1(OptionToIterableId, _, Call2(OptionMapId, _, instanceOf@IfInstanceOf(x), f: FuncExp[Any, _])))) =>
        e*/
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

  //The body moves the filter up one level, but we want to do it multiple times, as many as needed.
  //However, we needn't apply this optimization in a fixpoint loop: the optimization is applied bottom up, which is
  //exactly what we need!
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

  val mapToFlatMap: Exp[_] => Exp[_] = {
    import OptionOps._
    e => e match {
      case MapOp(c: Exp[Traversable[t]], f) =>
        c flatMap FuncExp.makefun(Seq(f.body), f.x)
      case Call2(OptionMapId, _, c: Exp[Option[t]], f: FuncExp[_, u]) =>
        c flatMap FuncExp.makefun(Some(f.body), f.x)
      case _ => e
    }
  }

  val flatMapToMap: Exp[_] => Exp[_] = {
    import OptionOps._
    e => e match {
      case Call2(OptionFlatMapId, _, c: Exp[Option[_ /*t*/]], f @ FuncExpBodyUntyped(ExpOption(Some(body: Exp[TraversableOnce[u]])))) =>
        c map FuncExp.makefun(body, f.x)
      case FlatMap(c: Exp[Traversable[t]], f @ FuncExpBody(ExpSeq(body))) =>
        c map FuncExp.makefun(body, f.x)
      case _ => e
    }
  }

  val letTransformer: Exp[_] => Exp[_] = {
    e => e match {
      case FlatMap(ExpSeq(v), f) => letExp(v)(f)
      case _ => e
    }
  }

  val normalizer: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
      case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
      case _ => e
    }

  private[optimization] def stripView[T](coll: Exp[Traversable[T]]) = stripViewUntyped(coll)

  //This type is incorrect whenever T is a view type. Be careful!
  private[optimization] def stripViewUntyped[T](coll: Exp[T]): Exp[T] =
    coll match {
      case View(coll2: Exp[Traversable[t]]) => coll2.asInstanceOf[Exp[T]]
      case _ => coll
    }
}

object Optimization {
  import scala.collection.mutable.Map

  val subqueries: Map[Exp[_], Any] = Map.empty

  def resetSubqueries() = subqueries.clear()
  def addSubquery[T](_query: Dummy[Exp[T]]) {
    val query = OptimizationTransforms.stripViewUntyped(_query.v)
    val optquery = optimizeIdx(query)
    val intQuery = optquery.interpret() //XXX: what if query is an incrementally maintained collection? We don't want to call interpret() again!

    //Let us ensure that both the unoptimized and the optimized version of the query are recognized by the
    // optimizer. TODO: Reconsider again whether this is a good idea.
    subqueries += normalize(query) -> intQuery
    subqueries += normalize(optquery) -> intQuery
  }

  def removeSubquery[T](_query: Dummy[Exp[T]]) {
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

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeIdentityMaps)

  def toTypeFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.toTypeFilter)

  def hoistFilter[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.hoistFilter)

  def removeRedundantOption[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.removeRedundantOption)

  def mapToFlatMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.mapToFlatMap)

  def flatMapToMap[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.flatMapToMap)

  def letTransformer[T](exp: Exp[T]): Exp[T] = exp.transform(OptimizationTransforms.letTransformer)

  def shareSubqueries[T](query: Exp[T]): Exp[T] =
    new SubquerySharing(subqueries).shareSubqueries(query)

  private def optimizeBase[T](exp: Exp[T]): Exp[T] =
    removeTrivialFilters(
      shareSubqueries(mapToFlatMap(
        removeTrivialFilters(
          removeIdentityMaps( //Do this again, in case maps became identity maps after reassociation
            reassociateOps(
              mergeMaps(
                mergeFilters(
                  hoistFilter( //Do this before merging filters!
                    cartProdToAntiJoin(
                      optimizeCartProdToJoin(
                        removeRedundantOption(toTypeFilter(
                          sizeToEmpty(
                            removeIdentityMaps(exp)))))))))))))))

  //The result of letTransformer is not understood by the index optimizer.
  //Therefore, we don't apply it at all on indexes, and we apply it to queries only after
  //subquery sharing.
  private def optimizeIdx[T](exp: Exp[T]): Exp[T] =
    flatMapToMap(optimizeBase(exp))

  def optimize[T](exp: Exp[T]): Exp[T] =
    flatMapToMap(letTransformer(optimizeBase(exp)))
}
