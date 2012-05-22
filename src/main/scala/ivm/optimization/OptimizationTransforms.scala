package ivm
package optimization

import expressiontree._
import Lifting._
import Numeric.Implicits._
import annotation.tailrec
import collection.mutable.Stack
import collection.TraversableLike
import OptimizationUtil._

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
                                             moFun: Fun[S, TResult], fmFun: Fun[T, Traversable[TResult]],
                                             wfFun: Fun[S, Boolean]): Exp[Traversable[TResult]] /*Join[T, S, TKey, TResult]*/ = {
    stripView(fmColl).join(
      stripView(wfColl))(
      Fun.makefun[T, TKey](lhs, fmFun.x).f,
      Fun.makefun[S, TKey](rhs, wfFun.x).f,
      Fun.makepairfun[T, S, TResult](
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
    case e@FlatMap(fmColl,
    fmFun@FuncExpBody(MapNode(Filter(filterColl, filterFun@FuncExpBody(Eq(lhs, rhs))), moFun)))
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

  val removeTrivialFilters: Exp[_] => Exp[_] = {
    case Filter(coll, FuncExpBody(Const(true))) =>
      stripViewUntyped(coll)
    case e => e
  }

  //Move constants on the left-side of a boolean connective.
  private val reassociateBoolOps: Exp[_] => Exp[_] = {
    case And(l, r@Const(_)) =>
      r && l
    case Or(l, r@Const(_)) =>
      r || l
    case e => e
  }

  //Some cases of constant folding for booleans.
  //The code could be optimized to save repeated matches on And and Or in the different functions, but that seems premature.
  val simplifyConditions: Exp[_] => Exp[_] =
    reassociateBoolOps andThen {
      case And(Const(true), x) => x
      case And(c@Const(false), x) => c
      case Or(Const(false), x) => x
      case Or(c@Const(true), x) => c
      case Not(Not(c)) => c
      case e => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] = {
    case MapNode(col, FuncExpIdentity()) =>
      col
    case e => e
  }

  /*
  private def removeIdentityMaps[T](e: Exp[T]): Exp[T] =
    e match {
      //Alternative 1 - a cast is required:
      case MapNode(col: Exp[_ /*T*/], FuncExpIdentity()) =>
        col.asInstanceOf[Exp[T]]
      //Alternative 2 - causes a warning, but works and is more elegant:
      case MapNode(col: Exp[T], FuncExpIdentity()) =>
        col
      //Possibility 2 is what is used in the Scala-virtualized tutorial.
      case e => e
    }
  */

  //Same problem also when fusing map and flatMap (which we don't do yet).
  private def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: Fun[T, U], g: Fun[U, V]): Exp[Traversable[V]] =
    coll.map(x => letExp(f(x))(g))

  //coll.map(f.f andThen g.f) //Old implementation, equivalent to inlining f(x) in the body of g. This might duplicate work!
  //coll.map(g.f andThen f.f) //Here the typechecker can reject this line.

  /*private def buildMergedFlatMap[T, U, V](coll: Exp[Traversable[T]], f: Fun[T, U], g: Fun[U, Traversable[V]]) =
    coll flatMap (x => letExp(f(x))(g))*/

  //Now we'd like a non-work-duplicating inliner. We'd need a linear type system as in GHC's inliner.

  val mergeMaps: Exp[_] => Exp[_] = {
    case MapNode(MapNode(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      buildMergedMaps(coll, f, g)
    //Since inner nodes were already optimized, coll will not be a MapNode node, hence we needn't call mergeMaps on the
    //result.
    //coll map (x => g(f(x)))
    //coll.map(Fun(f.andThen(g))) //Demonstrate norm-by-eval.
    case e => e
  }

  val mergeFlatMaps: Exp[_] => Exp[_] = {
    case FlatMap(MapNode(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(g)))
    case MapNode(FlatMap(coll, f), g) =>
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
      case MapNode(MapNode(coll, f1), f2) =>
        mergeMaps2(buildMergedMaps(coll, f1, f2)).
          //Note the need for this cast.
          asInstanceOf[Exp[T]]
      case _ => e
    }
  }

  val mergeMaps3: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case m: MapNode[t, repr, u, that] =>
        Util.assertType[Exp[repr]](m.base)
        (m.base map m.f)(m.c)
      //Outdated comments:
      //m.base.map[u, that](m.f)(m.c) //does not work for the same reason - m.base is considered as having type Exp[Traversable[t]]. That's however because of my implicit conversion, which is
      //rather limited.
      //MapNode[t, repr, u, that](m.base, m.f)(m.c) //works
      //MapNode(m.base, m.f)(m.c) //works
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
  // For instance, both (1 + x) + 2 and 1 + (x + 2) are rewritten to (1 + 2) + x
  // and then to 3 + x, conceptually (in practice,
  // such steps are fused together).
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
    case n@Negate(Const(c)) =>
      n.isNum.negate(c)
    case p@Plus(l, r) =>
      buildSum(l, r)(p.isNum)
    case t@Times(l, r) =>
      buildProd(l, r)(t.isNum)
    case e => e
  }

  val mergeFilters: Exp[_] => Exp[_] = {
    case e@Filter(col, f) =>
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

  //Fuse multiple views
  val mergeViews: Exp[_] => Exp[_] = {
    case View(coll@View(_)) =>
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

  private def buildTypeFilter[S, T, U](coll: Exp[Traversable[T]], classS: Class[S], f: Fun[S, Traversable[U]], origFmFun: Fun[T, Traversable[U]]): Exp[Traversable[U]] = {
    val res = coll.typeFilterClass(classS).flatMap(f.f)
    //Check that the transformed expression has overall the same type as the original one:
    Util.checkSameTypeAndRet(coll flatMap origFmFun)(res)
  }

  private def tryBuildTypeFilter[T, U](coll: Exp[Traversable[T]],
                                       fmFun: Fun[T, Traversable[U]],
                                       e: Exp[Traversable[U]]): Exp[Traversable[U]] = {
    val X = fmFun.x
    //Correct safety condition for this optimization: The variable of fmFun must appear always wrapped in the same
    //IfInstanceOf node (with the same type manifest...)
    val containingXParent = fmFun.body.findTotFun(_.children.flatMap(_.children).contains(X))
    val containingX = fmFun.body.findTotFun(_.children.contains(X))
    containingX.head match {
      case instanceOfNode@IfInstanceOf(X, _) if containingX.forall(_ == instanceOfNode) =>
        if (containingXParent.forall(_ == (instanceOfNode: Exp[Iterable[_]]))) {
          val v = Fun.gensym()
          val transformed = fmFun.body.substSubTerm(containingXParent.head, Seq(v))
          buildTypeFilter(coll, instanceOfNode.classS, Fun.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)
        } else {
          val v = Fun.gensym()
          val transformed = fmFun.body.substSubTerm(instanceOfNode, Some(v))
          //Note: on the result we would really like to drop all the 'Option'-ness, but that's a separate step.
          //Also, if we are in this branch, it means the client code is really using the 'Option'-ness of the value, say
          //via orElse or getOrElse, so we can't drop it.
          buildTypeFilter(coll, instanceOfNode.classS, Fun.makefun(transformed.asInstanceOf[Exp[Traversable[U]]], v), fmFun)
        }
      case _ =>
        e
    }
  }

  /*
  fmFun match {
    /*case FuncExpBody(Call1(`optionToIterableId`, _, Call2(`optionMapId`, _, instanceOf@IfInstanceOf(x), f: Fun[Any, _]))) =>
  buildTypeFilter(coll, instanceOf.cS, f)*/
   */

  val toTypeFilter: Exp[_] => Exp[_] = {
    case e@FlatMap(coll, fmFun: Fun[t, u]) =>
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
                                          fmFun: Fun[T, Traversable[U]],
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
        case MapNode(subColl, f) => isSupported(subColl, LetNode) && !f.body.isOrContains(X)
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
    case e@FlatMap(coll, (fmFun: Fun[_, Traversable[u]])) =>
      tryRemoveRedundantLet(coll, fmFun, e.asInstanceOf[Exp[Traversable[u]]])
    case e => e
  }

  private def buildHoistedFilterForFlatMap[T, U, V](coll1: Exp[Traversable[T]], fmFun: Fun[T, Traversable[V]],
                                                    coll2: Exp[Traversable[U]],
                                                    filterFun: Fun[U, Boolean],
                                                    fmFun2: Fun[U, Traversable[V]]): Exp[Traversable[V]] = {
    //Let's show that the source types are correct, in that we can rebuild the original expression:
    import Util.assertType
    assertType[Exp[Traversable[V]]](coll1.flatMap(fmFun.f))
    assertType[Exp[Traversable[V]]](coll1.flatMap(Fun.makefun(coll2.filter(filterFun.f).flatMap(fmFun2.f), fmFun.x).f))
    val ret: Exp[Traversable[V]] = coll1.withFilter(Fun.makefun(filterFun.body, fmFun.x).f) flatMap Fun.makefun(stripView(coll2) flatMap fmFun2.f, fmFun.x).f
    ret
  }

  //The body moves the filter up one level, but we want to do it multiple times, as many as needed.
  //However, we needn't apply this optimization in a fixpoint loop: the optimization is applied bottom up, which is
  //exactly what we need!
  //Scalac miscompiles this code if I write it the obvious way - without optimizations enabled!
  val hoistFilter: Exp[_] => Exp[_] = {
    case FlatMap(coll1, fmFun@FuncExpBody(FlatMap(Filter(coll2, filterFun), fmFun2)))
      if !filterFun.body.isOrContains(filterFun.x) =>
      buildHoistedFilterForFlatMap(coll1, fmFun, coll2, filterFun, fmFun2)
    case e => e
  }

  private def buildMapToFlatMap[T, U](c: Exp[Traversable[T]], f: Fun[T, U]): Exp[Traversable[U]] =
    c flatMap Fun.makefun(Seq(f.body), f.x)

  val mapToFlatMap: Exp[_] => Exp[_] = {
    case MapNode(c, f) =>
      buildMapToFlatMap(c, f)
    /*case Call2(OptionMapId, _, c: Exp[Option[t]], f: Fun[_, u]) =>
      c flatMap Fun.makefun(Some(f.body), f.x)*/
    case e => e
  }

  private def buildFlatMapToMap[T, U](c: Exp[Traversable[T]], body: Exp[U], f: Fun[T, Traversable[U]]): Exp[Traversable[U]] =
    c map Fun.makefun(body, f.x)

  val flatMapToMap: Exp[_] => Exp[_] = {
    case FlatMap(c, f@FuncExpBody(ExpSeq(Seq(body)))) =>
      buildFlatMapToMap(c, body, f)
    case e => e
  }

  def isTrivial(v: Exp[_]): Boolean = v match {
    case ExpSelection(_, _, e) => isTrivial(e)
    case e: ExpProduct with Exp[_] => e.children forall isTrivial
    case _: TypedVar[_] => true
    case _ => false
  }

  def subst[S, T](fun: Fun[S, T])(arg: Exp[S]) = {
    //See TAPL page 75 - we need this transformation whenever we might duplicate terms. XXX check if other
    // transformations duplicate terms. Map fusion uses letExp, to allow for a smarter inliner - I hope I was
    // consistent in doing this.
    fun(arg) transform {
      case fun: Fun[_, _] => Fun.rename(fun)
      case e => e
    }
  }

  //XXX: also reduce lets where the variable is only used once. Papers on the GHC inliner explain how to do this and why
  //a linear type system is needed for that.
  val letTransformerTrivial: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(v)), f) if isTrivial(v) => subst(f)(v)
    case e => e
  }

  //Bug-triggering variant of the code below.
  /*@tailrec private def usesArgAtMostOnce[S, T](f: Fun[S, T], v: Exp[_]): Boolean = {
    f match {
      case FuncExpBody(FlatMap(ExpSeq(Seq(v2)), g)) if !v2.isOrContains(v) =>
        usesArgAtMostOnce(g, v)
      case FuncExpBody(FlatMap(baseUsingV, g)) =>
        val occurrences = baseUsingV.findTotFun(_ == v)
        occurrences.length == 1 //gives spurious error
        //false
      case _ => false
    }
  }*/

  /*
   * Inlining: if you bind something and then use it immediately (1), it's used only once; the same if there are in between only other bindings, i.e. iterations over single-element sequences (try to make this into a complete criterion). Example:
for {
a <- Let(…)
b <- f(a)
…
}
In other words: FlatMap(Seq(a), x => FlatMap(f(a), g)) and combinations with Filter instead of FlatMap.
Let's assume that our binding of interest is bound in the head of a comprehension - otherwise it might be used more than once, but only
once per bound value. So let's consider
FlatMap(a, x => f(x)).
We want to characterize the subterms of f which are only evaluated once; in addition, we need that our variable appears in only one of them.
Theorem: if and only if a variable bound in a for-comprehension (using only FlatMap) is used at most once, then and only then it is used either in the
   */
  private def usesArgAtMostOnce[S, T](f: Fun[S, T]): Boolean = usesArgAtMostOnce(f, f.x)

  //Problem here: this assumes that the variable does not appear under explicit lambdas
  @tailrec private def usesArgAtMostOnce(f: Fun[_, _], v1: Exp[_]): Boolean = {
    f match {
      case FuncExpBody(FlatMap(ExpSeq(Seq(exp2)), g)) if !exp2.isOrContains(v1) =>
        //This allows to inline id1 in cases like:
        //v1 <- Let(exp1)
        //v2 <- Let(exp2) //v1 not free in exp2, that is, !v2.isOrContains(v1)
        //v3 <- Let(exp3) //v1 free in exp3, but not used any more; or v1 not free in exp3 but free in exp4; and so on.
        //v4 <- Let(exp4)
        //Note that it is crucial that all bindings act on single-element collections; we check that by looking for when this is statically known, i.e. we look for ExpSeq(Seq(_)), but we might want to extend that to
        //Const(seq) if seq has "at runtime" just one element.
        usesArgAtMostOnce(g, v1)
      case FuncExpBody(FlatMap(baseUsingV, g)) =>
        //Let's assume that unnesting has already happened, hence all functions we find are just function definitions and not nested FlatMaps to further analyze.
        //Since functions might be applied multiple times, we just make sure that nested function definitions do not refer to g.
        baseUsingV.findTotFun(_ == v1).length == 1 && // length < 1 was considered before.
          !g.isOrContains(v1) &&
          (baseUsingV.find {
            case f: Fun[_, _] => true
          } forall (!_.isOrContains(v1)))
      case _ => false //XXX add corresponding cases for Filter. Or add a common pattern-matcher encompassing both FlatMap and Filter, or more in general all available binding constructs!
    }
  }

  //This allows to inline definitions if they are used at most once.
  private[optimization] val letTransformerUsedAtMostOnce: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(exp1)), f) if usesArgAtMostOnce(f) => subst(f)(exp1)
    case e => e
  }

  //This is used only at the end, to convert the remaining Let()s into function applications; they are however
  //_not_ beta-reduced, so that computation is not duplicated.
  val letTransformer: Exp[_] => Exp[_] = {
    case FlatMap(ExpSeq(Seq(v)), f) => letExp(v)(f)
    case e => e
  }

  val deltaReductionTuple: PartialFunction[Exp[_], Exp[_]] = {
    case ExpSelection(arity, selected, e: ExpProduct) =>
      e.metaProductElement(selected - 1)
  }

  val betaReduction: PartialFunction[Exp[_], Exp[_]] = {
    //case a: App[s, t] => a.f(a.t) //causes a warning
    //To ensure termination, this must only apply if this rule changes behavior, that is, if App contains a Fun!
    //Otherwise fToFunOps will recreate a new App node.
    case appNode@App(fun: Fun[_, _], arg)
      //if ((body findTotFun (_ == fun.x)).length == 1) //Inlining side conditions. Damn, we need to use unrestricted inlining as here, simplify, and then use CSE again,
      //to have a robust solution.
    =>
      subst(fun)(arg)
  }

  //This is the shortest way of writing identity.
  val emptyTransform: PartialFunction[Exp[_], Exp[_]] = {
    case e => e
  }

  // Fixpoint iteration for beta-delta reduction :-). This works very well because:
  //   (f andThen g).isDefinedAt = f.isDefinedAt. (and g is supposed to be total - this code won't work so well if g is
  // in fact partial, another example of PartialFunction <: Function1 being totally confusing).

  // The recursion scheme here is remarkably similar to the one used with parser combinators:
  //   statements = (statement ~ statements) | empty
  // which can be shortened (IIRC) to:
  //   statements = statement*
  // Remember that Scala parser combinators are applicative parsers (i.e. Applicative and Alternative). Also note that
  // Alternative is described as "A monoid on applicative functors". Which nowadays is even obvious to me, oh my God!
  // Are my transformers an instance of the same structure?
  //Note: there's a reason parser combinators take their arguments by value; partial function combinators don't, hence we
  //suffer a lot
  //Note: recursive vals don't work - a recursive definition like this requires def.
  //val betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This recursive def does not work either - upon a call it tries to unfold itself.
  //def betaDeltaReducer: Exp[_] => Exp[_] = (deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse {case e => e} //that's the shortest way of writing identity.
  //This one always terminates, as long as the involved PartialFunctions are only defined when they do transform their input.
  //TODO: write a Kleene star to encapsulate this pattern; then lookup under what name is this technique already known.
  def betaDeltaReducer(exp: Exp[_]): Exp[_] = ((deltaReductionTuple orElse betaReduction) andThen betaDeltaReducer orElse emptyTransform)(exp)

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
    case FlatMap(Filter(c0, f@FuncExpBody(Not(IsEmpty(Filter(c, p))))), fmFun) =>
      //Since x0 = f.x, and fmFun = x1 => B, then [x1 |-> x0]B is (x1 => B) x0, that is fmFun(f.x), and restQuery = x => [x1 |-> x0]B =
      val restQuery = Fun.makefun(fmFun(f.x), p.x)
      //toSet remove any duplicates to preserve semantics; in the expression c exists p, p might be true for more elements of c. When unnesting,
      //we produce c withFilter p, and for each element in the result we apply x1 => B. Instead, with toSet we unify the results after applying
      //filtering. That's unsatisfactory though; we could instead of using toSet on the result, use breakout as CanBuildFrom instance on the last flatMap.
      //A problem, in both cases, is that the result of this transformation looks slower than the original query.
      //Is duplicate elimination after applying restQuery valid? I guess not: the flatMapped function might just produce an element multiple times.
      //So we produce instead a Set of booleans to do duplicate elimination and then filter with identity!
      //XXX untested.
      stripView(c0) flatMap Fun.makefun((((stripView(c) map p)(collection.breakOut): Exp[Set[Boolean]]) filter identity flatMap restQuery)(collection.breakOut): Exp[Traversable[Any]], f.x)
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
      collEp flatMap Fun.makefun(letExp(ep)(fy), fxp.x)
    results in:
[error] /Users/pgiarrusso/Documents/Research/Sorgenti/linqonsteroids/src/main/scala/ivm/optimization/Optimization.scala:536: value flatMap is not a member of ivm.expressiontree.Exp[Any]
[error]       collEp flatMap Fun.makefun(letExp(ep)(fy), fxp.x)
[error]              ^
      * However, scalac seems "right": collEp has type Exp[Repr], which apparently erases to Exp[Any] even if a type bound _is_ given.
      * XXX report this as another bug.
      */
    //Why and when do we call generalUnnesting again?
    //Subexpressions are already optimized, but subexpressions we build are not. In particular, if ep is a FlatMap node, when we invoke flatMap/filter on ep the result might require further unnesting.
    /*case FlatMap(FlatMap(collEp, fxp @ FuncExpBody(ep)), fy @ FuncExpBody(e)) =>
      collEp flatMap Fun.makefun(letExp(ep)(fy.f), fxp.x)*/
    case FlatMap(FlatMap(collEp, fxp@FuncExpBody(ep)), fy@FuncExpBody(e)) =>
      collEp flatMap Fun.makefun(generalUnnesting(ep flatMap fy).asInstanceOf[Exp[Traversable[Any]]], fxp.x).f
    //collEp flatMap Fun.makefun(Seq(ep) map (fy)/*Seq(ep) map fy*/, fxp.x).f
    case Filter(FlatMap(collEp, fxp@FuncExpBody(ep)), fy@FuncExpBody(e)) =>
      //collEp filter Fun.makefun(letExp(ep)(fy), fxp.x)
      collEp flatMap Fun.makefun(generalUnnesting(ep filter app(fy)).asInstanceOf[Exp[Traversable[Any]]], fxp.x)
    case e => e
  }

  /*
   * Consider:
   * coll1 flatMap (x1 => coll2 map (x2 => (x1, x2)) filter (pair => test(pair._1))
   * Conceptually, here the filter might be hoisted because it only depends on x1. But that's not visible unless we
   * merge the map and the filter, fuse their functions, and extract the filter again with transformedFilterToFilter.
   */
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    case Filter(MapNode(coll, mapFun), pred) => //This case should have higher priority, it seems more useful.
      coll flatMap Fun.makefun(letExp(mapFun.body)(Fun.makefun(if_#(pred.body)(Seq(pred.x)) else_# (Seq.empty), pred.x)), mapFun.x)
    //We preserve sharing here with letExp; currently, subsequent stages will do indiscriminate inlining and replicate the map, but
    //the inliner will later be improved.
    //This case, together with inlinng and transformedFilterToFilter, is equivalent to what Tillmann suggested and
    //then dismissed.
    /*
case MapNode(Filter(coll, pred @ FuncExpBody(test)), mapFun) =>
  //This transformation cancels with transformedFilterToFilter + flatMapToMap. Not sure if it's ever useful.
  //It could be useful as a separate stage, if this turns out to be a faster implementation.
  //After this optimization, we need to float the if_# to around the body of mapFun
  //coll flatMap (Fun.makefun(if_# (test)(Seq(pred.x)) else_# {Seq.empty}, pred.x) andThen mapFun)
  //Let's do this directly:
  coll flatMap Fun.makefun(
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
    case FlatMap(coll, fmFun@FuncExpBody(IfThenElse(test, thenBranch, elseBranch@ExpSeq(Seq())))) =>
      coll filter Fun.makefun(test, fmFun.x) flatMap Fun.makefun(thenBranch, fmFun.x)
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
