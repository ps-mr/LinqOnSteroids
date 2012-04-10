package ivm
package optimization

import expressiontree._
import Lifting._
import Numeric.Implicits._
import annotation.tailrec
import collection.mutable.Stack
import collection.TraversableLike

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
                                                  moFun: FuncExp[S, TResult], fmFun: FuncExp[T, Traversable[TResult]],
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
  val cartProdToJoin: Exp[_] => Exp[_] = {
    case e @ FlatMap(fmColl,
      fmFun @ FuncExpBody(MapOp(Filter(filterColl, filterFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
      if !filterColl.isOrContains(fmFun.x)
    =>
      if (!(lhs.isOrContains(filterFun.x)) && !(rhs.isOrContains(fmFun.x)))
        buildJoin(fmColl, filterColl, lhs, rhs, moFun, fmFun, filterFun)
      else if (!(rhs.isOrContains(filterFun.x)) && !(lhs.isOrContains(fmFun.x)))
        buildJoin(fmColl, filterColl, rhs, lhs, moFun, fmFun, filterFun)
      else
        e
    case e => e
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

  val cartProdToAntiJoin: Exp[_] => Exp[_] = {
    case e @ Filter(filteredColl,
      filterFun @ FuncExpBody(Forall(forallColl, forallFun @ FuncExpBody(Not(Eq(lhs, rhs))))))
    //case FlatMap(fmColl,
      //fmFun @ FuncExpBody(MapOp(Filter(filterColl, filterFun @ FuncExpBody(Not(Eq(lhs, rhs)))), moFun)))
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

  val removeTrivialFilters: Exp[_] => Exp[_] = {
    case Filter(coll, FuncExpBody(Const(true))) =>
      stripViewUntyped(coll)
    case e => e
  }

  //Move constants on the left-side of a boolean connective.
  private val reassociateBoolOps: Exp[_] => Exp[_] = {
    case And(l, r @ Const(_)) =>
      r && l
    case Or(l, r @ Const(_)) =>
      r || l
    case e => e
  }

  //Some cases of constant folding for booleans.
  //The code could be optimized to save repeated matches on And and Or in the different functions, but that seems premature.
  val simplifyConditions: Exp[_] => Exp[_] =
    reassociateBoolOps andThen {
      case And(Const(true), x) => x
      case And(c @ Const(false), x) => c
      case Or(Const(false), x) => x
      case Or(c @ Const(true), x) => c
      case Not(Not(c)) => c
      case e => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] = {
    case MapOp(col, FuncExpIdentity()) =>
      col
    case e => e
  }

  /*
  private def removeIdentityMaps[T](e: Exp[T]): Exp[T] =
    e match {
      //Alternative 1 - a cast is required:
      case MapOp(col: Exp[_ /*T*/], FuncExpIdentity()) =>
        col.asInstanceOf[Exp[T]]
      //Alternative 2 - causes a warning, but works and is more elegant:
      case MapOp(col: Exp[T], FuncExpIdentity()) =>
        col
      //Possibility 2 is what is used in the Scala-virtualized tutorial.
      case e => e
    }
  */

  //Same problem also when fusing map and flatMap (which we don't do yet).
  private def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: FuncExp[T, U], g: FuncExp[U, V]): Exp[Traversable[V]] =
    coll.map(x => letExp(f(x))(g))
    //coll.map(f.f andThen g.f) //Old implementation, equivalent to inlining f(x) in the body of g. This might duplicate work!
    //coll.map(g.f andThen f.f) //Here the typechecker can reject this line.

  /*private def buildMergedFlatMap[T, U, V](coll: Exp[Traversable[T]], f: FuncExp[T, U], g: FuncExp[U, Traversable[V]]) =
    coll flatMap (x => letExp(f(x))(g))*/

  //Now we'd like a non-work-duplicating inliner. We'd need a linear type system as in GHC's inliner.

  val mergeMaps: Exp[_] => Exp[_] = {
    case MapOp(MapOp(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      buildMergedMaps(coll, f, g)
      //Since inner nodes were already optimized, coll will not be a MapOp node, hence we needn't call mergeMaps on the
      //result.
    case e => e
  }

  val mergeFlatMaps: Exp[_] => Exp[_] = {
    case FlatMap(MapOp(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(g)))
    case MapOp(FlatMap(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(_ map g)))
    case e => e
  }

  //Express in the type system that transformations need to preserve typing:
  trait Transformer {
    def apply[T](e: Exp[T]): Exp[T]
  }
  val mergeMaps2: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case MapOp(MapOp(coll, f1), f2) =>
        mergeMaps2(buildMergedMaps(coll, f1, f2)).
          //Note the need for this cast.
          asInstanceOf[Exp[T]]
      case _ => e
    }
  }

  val mergeMaps3: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case m: MapOp[t, repr, u, that] =>
        Util.assertType[Exp[repr]](m.base)
        (m.base map m.f)(m.c)
        //Outdated comments:
        //m.base.map[u, that](m.f)(m.c) //does not work for the same reason - m.base is considered as having type Exp[Traversable[t]]. That's however because of my implicit conversion, which is
        //rather limited.
        //MapOp[t, repr, u, that](m.base, m.f)(m.c) //works
        //MapOp(m.base, m.f)(m.c) //works
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

  val reassociateOps: Exp[_] => Exp[_] = {
    case n @ Negate(Const(c)) =>
      n.isNum.negate(c)
    case p@Plus(l, r) =>
      buildSum(l, r)(p.isNum)
    case t@Times(l, r) =>
      buildProd(l, r)(t.isNum)
    case e => e
  }

  val mergeFilters: Exp[_] => Exp[_] = {
    case e @ Filter(col, f) =>
      stripViewUntyped(col) match {
        case Filter(col2, f2) =>
          mergeFilters(
            col2 withFilter {
              (x: Exp[_]) => And(f2(x), f(x))
            })
        case _ => e
      }
    case e => e
  }

  //Recognize relational-algebra set operations; they can be executed more efficiently if one of the two members is indexed.
  //However, sets are already always indexed, so these optimizations will not have a huge impact by themselves.
  val setIntersection: Exp[_] => Exp[_] = {
    case e @ Filter(col, predFun @ FuncExpBody(Contains(col2, x))) if (x == predFun.x) =>
      //XXX transformation not implemented
      e //col.join(col2)(identity, identity, _._1) //Somewhat expensive implementation of intersection.
      //e //Intersect(col, col2)
    case e => e
  }

  //We want to support anti-joins. Is this one? This is an anti-join where a set is involved and with identity selectors.
  val setDifference: Exp[_] => Exp[_] = {
    case e @ Filter(col, predFun @ FuncExpBody(Not(Contains(col2, x)))) if (x == predFun.x) =>
      //XXX transformation not implemented
      e //Diff(col, col2) //We cannot use Diff because col is not a Set - but we can build a more complex operator for this case.
    case e => e
  }

  //Fuse multiple views
  val mergeViews: Exp[_] => Exp[_] = {
    case View(coll @ View(_)) =>
      coll
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

  private def buildTypeFilter[S, T, U](coll: Exp[Traversable[T]], classS: Class[S], f: FuncExp[S, Traversable[U]], origFmFun: FuncExp[T, Traversable[U]]): Exp[Traversable[U]] = {
    val res = coll.typeFilterClass(classS).flatMap(f.f)
    //Check that the transformed expression has overall the same type as the original one:
    Util.checkSameTypeAndRet(coll flatMap origFmFun)(res)
  }

  private def tryBuildTypeFilter[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FuncExp[T, Traversable[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val X = fmFun.x
    //Correct safety condition for this optimization: The variable of fmFun must appear always wrapped in the same
    //IfInstanceOf node (with the same type manifest...)
    val containingXParent = fmFun.body.findTotFun(_.children.flatMap(_.children).contains(X))
    val containingX = fmFun.body.findTotFun(_.children.contains(X))
    containingX.head match {
      case instanceOfNode@IfInstanceOf(X, _) if containingX.forall(_ == instanceOfNode) =>
        if (containingXParent.forall(_ == (instanceOfNode: Exp[Iterable[_]]))) {
          val v = FuncExp.gensym()
          val transformed = fmFun.body.substSubTerm(containingXParent.head, Seq(v))
          buildTypeFilter(coll, instanceOfNode.classS, FuncExp.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)
        } else {
          val v = FuncExp.gensym()
          val transformed = fmFun.body.substSubTerm(instanceOfNode, Some(v))
          //Note: on the result we would really like to drop all the 'Option'-ness, but that's a separate step.
          //Also, if we are in this branch, it means the client code is really using the 'Option'-ness of the value, say
          //via orElse or getOrElse, so we can't drop it.
          buildTypeFilter(coll, instanceOfNode.classS, FuncExp.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)
        }
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
    case e @ FlatMap(coll, fmFun: FuncExp[t, u]) =>
      tryBuildTypeFilter(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
    case e => e
  }

  //removeRedundantLet is supposed to eliminate redundant lets from code like:
  //for (i <- base.typeFilter[Int]; j <- Let(i) if j % 2 ==# 0) yield j
  //which is for instance produced by toTypeFilter.
  //The transformation can be described as FlatMap(coll, x => f(Seq(x))) => f(coll), under the condition that
  //f is a sequence homomorphism, i.e. if it distributes over list concatenation so that
  // coll flatMap (x => f(Seq(x))) = {flatMap identity}
  // coll map (x => f(Seq(x))) flatten = {undo map fusion}
  // coll map (x => Seq(x)) map f flatten = {distributivity of f}
  // f(coll map (x => Seq(x)) flatten = {compose map and flatten}
  // f(coll)
  // However, we use in practice a much more restrictive condition.
  //This optimization does not extend to normal Let bindings as used in FindBugsAnalyses. There we need to produce the usual
  //desugaring of Let - i.e. to use letExp; that's done in letTransformer.
  private def tryRemoveRedundantLet[T, U](coll: Exp[Traversable[T]],
                                       fmFun: FuncExp[T, Traversable[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val insideConv = fmFun.body
    // The safety condition for this optimization is two-fold:
    // 1. The variable of fmFun must appear always wrapped in the same
    //    Let node
    // 2. Only supported Traversable operations must appear.

    val X = fmFun.x

    //Check safety condition, part 2.
    //@tailrec
    def isSupported(insideConv: Exp[_], LetNode: Exp[_]): Boolean =
      insideConv match {
        case MapOp(subColl, f) => isSupported(subColl, LetNode) && !f.body.isOrContains(X)
        case Filter(subColl, f) => isSupported(stripViewUntyped(subColl), LetNode) && !f.body.isOrContains(X)
        case FlatMap(subColl, f) => isSupported(subColl, LetNode) && !f.body.isOrContains(X)
        case LetNode => true
        case _ => false
      }

    val containingX = insideConv.findTotFun(_.children.contains(X))
    containingX.head match {
      case letNode@ExpSeq(Seq(X)) if containingX.forall(_ == letNode) && isSupported(insideConv, letNode) =>
        insideConv.substSubTerm(letNode, coll)
      case _ =>
        e
    }
  }


  val removeRedundantOption: Exp[_] => Exp[_] = {
    case e @ FlatMap(coll, (fmFun: FuncExp[_, Traversable[u]])) =>
      tryRemoveRedundantLet(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
    case e => e
  }

  private def buildHoistedFilterForFlatMap[T, U, V](coll1: Exp[Traversable[T]], fmFun: FuncExp[T, Traversable[V]],
                                                coll2: Exp[Traversable[U]],
                                                filterFun: FuncExp[U, Boolean],
                                                fmFun2: FuncExp[U, Traversable[V]]): Exp[Traversable[V]] = {
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
  val hoistFilter: Exp[_] => Exp[_] = {
    case FlatMap(coll1, fmFun @ FuncExpBody(FlatMap(Filter(coll2, filterFun), fmFun2)))
      if !filterFun.body.isOrContains(filterFun.x) =>
      buildHoistedFilterForFlatMap(coll1, fmFun, coll2, filterFun, fmFun2)
    case e => e
  }

  private def buildMapToFlatMap[T, U](c: Exp[Traversable[T]], f: FuncExp[T, U]): Exp[Traversable[U]] =
    c flatMap FuncExp.makefun(Seq(f.body), f.x)

  val mapToFlatMap: Exp[_] => Exp[_] = {
    case MapOp(c, f) =>
      buildMapToFlatMap(c, f)
    /*case Call2(OptionMapId, _, c: Exp[Option[t]], f: FuncExp[_, u]) =>
      c flatMap FuncExp.makefun(Some(f.body), f.x)*/
    case e => e
  }

  private def buildFlatMapToMap[T, U](c: Exp[Traversable[T]], body: Exp[U], f: FuncExp[T, Traversable[U]]): Exp[Traversable[U]] =
    c map FuncExp.makefun(body, f.x)

  val flatMapToMap: Exp[_] => Exp[_] = {
    case FlatMap(c, f @ FuncExpBody(ExpSeq(Seq(body)))) =>
      buildFlatMapToMap(c, body, f)
    case e => e
  }

  val letTransformer: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(v)), f) => letExp(v)(f)
    case e => e
  }

  val deltaReductionTuple: PartialFunction[Exp[_], Exp[_]] = {
    case ExpSelection(arity, selected, e: ExpProduct) => e.metaProductElement(selected - 1)
  }

  val betaReduction: PartialFunction[Exp[_], Exp[_]] = {
    //case a: App[s, t] => a.f(a.t) //causes a warning
    case appNode @ App(fun /* @ FuncExpBody(body)*/, arg)
      //if ((body findTotFun (_ == fun.x)).length == 1) //Inlining side conditions. Damn, we need to use unrestricted inlining as here, simplify, and then use CSE again,
      //to have a robust solution.
    =>
      fun(arg)
  }

  val betaDeltaReducer: Exp[_] => Exp[_] = deltaReductionTuple orElse betaReduction orElse {case e => e} //that's the shortest way of writing identity.
  /*val betaReduction: Exp[_] => Exp[_] = {
    case a: App[t, u] => a.f(a.t)
    case ExpSelection(arity, selected, e: ExpProduct) => e.metaProductElement(selected - 1)
    case e => e
  }*/

  val existsUnnester: Exp[_] => Exp[_] = {
    //Rule N9 page 474 in Optimizing Object Queries Using an Effective Calculus, Fegaras and Maier, 2000:
    //c0 withFilter (x0 => c exists (x => p(x))) flatMap (x1 => B [not free in x]) |=> c0 flatMap (x0 => c withFilter (x => p(x)) flatMap restQuery)
    //where restQuery = x => [x1 |-> x0]B
    //However, that's only valid if the return value of the expression is an idempotent monoid, as stated there. We can workaround that by converting
    //the result of flatMap to a set.
    case FlatMap(Filter(c0, f @ FuncExpBody(Not(IsEmpty(Filter(c, p))))), fmFun) =>
      //Since x0 = f.x, and fmFun = x1 => B, then [x1 |-> x0]B is (x1 => B) x0, that is fmFun(f.x), and restQuery = x => [x1 |-> x0]B =
      val restQuery = FuncExp.makefun(fmFun(f.x), p.x)
      //toSet remove any duplicates to preserve semantics; in the expression c exists p, p might be true for more elements of c. When unnesting,
      //we produce c withFilter p, and for each element in the result we apply x1 => B. Instead, with toSet we unify the results after applying
      //filtering. That's unsatisfactory though; we could instead of using toSet on the result, use breakout as CanBuildFrom instance on the last flatMap.
      //A problem, in both cases, is that the result of this transformation looks slower than the original query.
      //Is duplicate elimination after applying restQuery valid? I guess not: the flatMapped function might just produce an element multiple times.
      //So we produce instead a Set of booleans to do duplicate elimination and then filter with identity!
      //XXX untested.
      stripView(c0) flatMap FuncExp.makefun((((stripView(c) map p)(collection.breakOut): Exp[Set[Boolean]]) filter identity flatMap restQuery)(collection.breakOut): Exp[Traversable[Any]], f.x)
    case e => e
  }

  //XXX: an extra safety condition is that we must reject a nested collection with stronger algebraic laws;
  //in the monoid comprehension calculus this is required for an expression to be _syntactically_ valid, but here it is
  //not.
  //For instance, this code would unnest a subquery creating a set (hence performing duplicate elimination) nested into
  //a query creating a list.
  val generalUnnesting: Exp[_] => Exp[_] = {
    /*
     * v = E' map (x' => e')
     * v flatMap (y => e) |-> E ' flatMap (x' => letExp(e')(y => e))
     * v filter (y => p) |-> ...
     */
    /* A somewhat interesting compile error:
    case FlatMap(FlatMap(collEp, fxp @ FuncExpBody(ep)), fy @ FuncExpBody(e)) =>
      collEp flatMap FuncExp.makefun(letExp(ep)(fy), fxp.x)
    results in:
[error] /Users/pgiarrusso/Documents/Research/Sorgenti/linqonsteroids/src/main/scala/ivm/optimization/Optimization.scala:536: value flatMap is not a member of ivm.expressiontree.Exp[Any]
[error]       collEp flatMap FuncExp.makefun(letExp(ep)(fy), fxp.x)
[error]              ^
      * However, scalac seems "right": collEp has type Exp[Repr], which apparently erases to Exp[Any] even if a type bound _is_ given.
      * XXX report this as another bug.
      */
    /*case FlatMap(FlatMap(collEp, fxp @ FuncExpBody(ep)), fy @ FuncExpBody(e)) =>
      collEp flatMap FuncExp.makefun(letExp(ep)(fy.f), fxp.x)*/
    case FlatMap(FlatMap(collEp, fxp @ FuncExpBody(ep)), fy @ FuncExpBody(e)) =>
      collEp flatMap FuncExp.makefun(ep flatMap fy, fxp.x).f
      //collEp flatMap FuncExp.makefun(Seq(ep) map (fy)/*Seq(ep) map fy*/, fxp.x).f
    case Filter(FlatMap(collEp, fxp @ FuncExpBody(ep)), fy @ FuncExpBody(e)) =>
      //collEp filter FuncExp.makefun(letExp(ep)(fy), fxp.x)
      collEp flatMap FuncExp.makefun(ep filter app(fy), fxp.x)
    case e => e
  }

  /*
   * Consider:
   * coll1 flatMap (x1 => coll2 map (x2 => (x1, x2)) filter (pair => test(pair._1))
   * Conceptually, here the filter might be hoisted because it only depends on x1. But that's not visible unless we
   * merge the map and the filter, fuse their functions, and extract the filter again with transformedFilterToFilter.
   */
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    case Filter(MapOp(coll, mapFun), pred) => //This case should have higher priority, it seems more useful.
      coll flatMap FuncExp.makefun(letExp(mapFun.body)(FuncExp.makefun(if_# (pred.body) (Seq(pred.x)) else_# (Seq.empty), pred.x)), mapFun.x)
      //We preserve sharing here with letExp; currently, subsequent stages will do indiscriminate inlining and replicate the map, but
      //the inliner will later be improved.
      //This case, together with inlinng and transformedFilterToFilter, is equivalent to what Tillmann suggested and
      //then dismissed.
      /*
    case MapOp(Filter(coll, pred @ FuncExpBody(test)), mapFun) =>
      //This transformation cancels with transformedFilterToFilter + flatMapToMap. Not sure if it's ever useful.
      //It could be useful as a separate stage, if this turns out to be a faster implementation.
      //After this optimization, we need to float the if_# to around the body of mapFun
      //coll flatMap (FuncExp.makefun(if_# (test)(Seq(pred.x)) else_# {Seq.empty}, pred.x) andThen mapFun)
      //Let's do this directly:
      coll flatMap FuncExp.makefun(
        if_# (test) {
          Seq(mapFun(pred.x))
        } else_# {
          Seq.empty
        }, pred.x)
        */
    case e => e
  }

  val transformedFilterToFilter: Exp[_] => Exp[_] = {
    //case FlatMap(coll, fmFun @ FuncExpBody(IfThenElse(test, thenBranch @ ExpSeq(Seq(element)), elseBranch @ ExpSeq(Seq())))) =>
      //coll filter
    case FlatMap(coll, fmFun @ FuncExpBody(IfThenElse(test, thenBranch, elseBranch @ ExpSeq(Seq())))) =>
      coll filter FuncExp.makefun(test, fmFun.x) flatMap FuncExp.makefun(thenBranch, fmFun.x)
    case e => e
  }

  val simplifyForceView: Exp[_] => Exp[_] = {
    case View(Force(coll)) => coll
    case Force(View(coll)) => coll
    case e => e
  }

  // Add view and force around Filter to imitate withFilter.
  // Transformation rule:
  // coll.filter(f).*map(g) -> coll.view.filter(f).*map(g).force
  // where *map stands for map or flatMap (the same on both sides).
  //Note: this assumes that maps have been converted to flatMaps.
  val filterToWithFilter: Exp[_] => Exp[_] = {
    case FlatMap(Filter(coll, p), f) =>
      (stripView(coll).view filter p flatMap f).force
    case e => e
  }

  val normalizer: Exp[_] => Exp[_] = {
    case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
    case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
    case e => e
  }

  /*private[optimization]*/ private[ivm] def stripView[T](coll: Exp[Traversable[T]]) = stripViewUntyped(coll)

  //This type is incorrect whenever T is a view type. Be careful!
  private[optimization] def stripViewUntyped[T](coll: Exp[T]): Exp[T] =
    coll match {
      case View(coll2) => coll2.asInstanceOf[Exp[T]]
      case _ => coll
    }
}

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

  //Call this whenever new MapOp nodes might be created, to simplify them if needed.
  //Requires map+flatMap normal form
  def handleNewMaps[T](exp: Exp[T]): Exp[T] =
    removeIdentityMaps( //Do this again, in case maps became identity maps after reassociation
      reassociateOps(betaDeltaReducer(
        mergeMaps(exp))))

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
                        flatMapToMap(sizeToEmpty(generalUnnesting(mapToFlatMap(
                          removeIdentityMaps(betaDeltaReducer(exp))))))))))))))))))))))) //the call to reducer is to test.

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
