package ivm.optimization

import ivm.expressiontree._
import Lifting._
import Util.assertType
import collection.mutable.ArrayBuffer
import CollectionUtils.collectFirst
import collection.{TraversableLike, Map}
import ivm.collections.TypeMapping

// Contract: Each map entry has the form Exp[T] -> T for some T
/**
 * Note: this code is not allowed to create expression nodes through their constructors, as with {@see Optimization}.
 * The subqueries map should be called the "precomputed query repository", and is a generalization of a repository of
 * indexes.
 */
object SubquerySharing {
  /*private*/ def println(msg: Any) {
    if (Optimization.isDebugLogEnabled)
      Predef.println(msg)
  }

  def residualQuery[T](e: Exp[Traversable[T]], conds: Set[Exp[Boolean]], v: TypedVar[_ /*T*/]): Exp[Traversable[T]] = {
    val residualcond: Exp[Boolean] = conds.fold(Const(true))(And)
    //Note that withFilter will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
    // instance from the HOAS representation produced.
    e withFilter FuncExp.makefun[T, Boolean](residualcond, v)
  }
}

class SubquerySharing(val subqueries: Map[Exp[_], Any]) {
  import SubquerySharing._
  val directsubqueryShare: Exp[_] => Exp[_] =
    e => subqueries.get(Optimization.normalize(e)) match {
      case Some(t) => asExp(t)
      case None => e
    }

  private def groupByShareBody[T, U](c: Exp[Traversable[T]],
                                      fx: Var,
                                      fEqBody: Eq[U], constantEqSide: Exp[U], varEqSide: Exp[U]) = {
    val groupedBy = c.groupBy[U](FuncExp.makefun[T, U](varEqSide, fx).f)

    assertType[Exp[U => Traversable[T]]](groupedBy) //Just for documentation.
    val toLookup = Optimization.normalize(groupedBy)
    subqueries.get(toLookup) match {
      //Note: x flatMap identity, on x: Option[Seq[T]], implements monadic join. We could also use x getOrElse Traversable.empty.
      //In both cases, the type of the resulting expression becomes Traversable, which might lead to the result having the wrong dynamic type.
      //Luckily, the CanBuildFrom instances will be too generic but will delegate their work to the source collection,
      //hence producing a result of the right dynamic type.
      case Some(t) =>
        println("Found simple index of form " + toLookup)
        Some(asExp(t.asInstanceOf[Map[U, Traversable[T]]]) get constantEqSide flatMap identity)
      case None =>
        println("Found no simple index of form " + toLookup)
        None
    }
  }

  private def tryGroupBy[T](c: Exp[Traversable[T]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var)
                           (cond: Exp[Boolean]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[u] =>
        val oq: Option[Exp[Traversable[T]]] =
          if (eq.t1.isOrContains(fx) && !eq.t2.isOrContains(fx))
            groupByShareBody[T, u](c, fx, eq, eq.t2, eq.t1)
          else if (eq.t2.isOrContains(fx) && !eq.t1.isOrContains(fx))
            groupByShareBody[T, u](c, fx, eq, eq.t1, eq.t2)
          else None
        oq.map(e => residualQuery(e, allConds - eq, fx))
      case _ => None
    }

  //We have to strip View if needed on _both_ sides before performing the match, to increase the chance of a match.
  //This is done here on one side, and on Optimization.addSubquery on the other side. Note that only the top-level strip
  //is visible.
  //Rewrite (if possible) coll.withFilter(elem => F[elem] ==# k && OtherConds[elem]) to (coll.groupBy(elem => F[elem]))(k).withFilter(x => OtherConds[x]),
  //with F and OtherConds expression contexts and under the condition that coll.groupBy(f) is already available as a precomputed subquery (i.e. an index).
  val groupByShare: Exp[_] => Exp[_] = {
    case e @ Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
      val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
      val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupBy(OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]]), conds, f.x)(_))
      optimized.getOrElse(e)
    case e => e
  }

  /*
  sealed trait FoundNode
  //TODO: Add  variants of FoundFilter for typeFilter and typeCase. When optimized, we can transform typeFilter into a stupid typecase and viceversa.
  case class FoundFilter[T](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Boolean], isOption: Boolean = false) extends FoundNode
  case class FoundFlatMap[T, U](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Traversable[U]], isOption: Boolean = false) extends FoundNode

  def defUseFVars(fvContains: Var => Boolean)(e: Exp[_]) = e.findTotFun { case v: Var => fvContains(v); case _ => false }.nonEmpty

  //Preconditions:
  //Postconditions: when extracting Some((parentNode, parentF)), filterExp, conds, foundEqs, allFVSeq, parentNode is a FlatMap node which
  // contains a filter expression as base collection (and not through a FuncExp node); the filter expression is returned, after being transformed
  // to a FoundFilter instance, inside filterExp. filterExp.isOption tells whether both filterExp and parentNode are of type Exp[Option[_]] instead of
  //Exp[Traversable[_]] - they cannot differ in this regard (currently). If we allow for conversion nodes later, this assumption will not hold
  //any more - we should normalize away such conversions if possible.
  def lookupEq(parent: Option[(Exp[Traversable[_]], FuncExp[_, _])],
               e: Exp[_],
               freeVars: Set[Var] = Set.empty,
               fvSeq: Seq[Var] = Seq.empty): Seq[((Exp[Traversable[_]], FuncExp[_, _]), FoundFilter[_], Set[Exp[Boolean]], Set[Eq[_]], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)

    val matchResult: Either[Exp[_], FoundNode] = e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) if parent.isDefined =>
        Right(FoundFilter(Right(c), f))
      case FlatMap(c: Exp[Traversable[_]], f: FuncExp[t, Traversable[u]]) =>
        Right(FoundFlatMap(Right(c), f))
      case _ =>
        Left(e)
    }

    matchResult.fold(
      {
        case f: FuncExp[_, _] =>
          lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case exp => exp.children flatMap {
          lookupEq(None, _, freeVars, fvSeq)
        }
      },
      {
        case ff @ FoundFilter(_ /*: Exp[Traversable[_ /*t*/]]*/, f: FuncExp[t, _ /*Boolean*/], _) =>
          assert (parent.isDefined)
          val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
          val allFreeVars: Set[Var] = freeVars + f.x
          val usesFVars = defUseFVars(allFreeVars) _

          //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
          //I managed, but it was beyond the abilities of type inference.
          val foundEqs =
            conds.map {
              case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesFVars(l) && !usesFVars(r) || usesFVars(r) && !usesFVars(l)) =>
                Seq(eq)
              case _ => Seq.empty
            }.fold(Seq.empty)(_ union _).toSet[Eq[_]]
          Seq((parent.get, ff, conds, foundEqs, fvSeq /*allFVSeq*/)) //Don't include the variable of the filter, which is going to be dropped anyway - hence fvSeq, not allFVSeq
        //case FlatMap(c, f: FuncExp[t, Traversable[u]]) =>
        //lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c, freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case FoundFlatMap(c, f: FuncExp[t, Traversable[u]], _) =>
          //Add all free variables bound in the location.
          lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c.fold(identity, identity), freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
      })
  }

  /*
   * When looking up an index, a filter in the original query will be preserved in the lookup key, and it might make the
   * lookup fail. If an index exists without a filter, we should try to use it.
   * Some filters might be required to prevent subsequent operations from failing, hence they might be required also in
   * the index; stripping the filter will thus create a query which would fail at runtime.
   * That is not a problem, since we don't execute the queries we build, just look them up; if the lookup is successful,
   * it means we constructed a query which the user added to the precomputed query repository, hence it better be safe.
   * An actual concern is that we might need to strip only some indexes but not others from the query in order to find a
   * matching index.
   */
  private def withoutFilters[TupleT, U](indexBaseToLookup: Exp[Traversable[TupleT]], tuplingTransform: (Exp[U], TypedVar[TupleT]) => Exp[U], fx: TypedVar[TupleT]): (Exp[Traversable[TupleT]], Exp[Boolean]) = {
    val v = ArrayBuffer[Exp[Boolean]]()
    //We are removing too many filters, and not keeping track of their bindings :-(.
    val res = indexBaseToLookup transform { //We only want to recognize filtering in the collection branch of FlatMap nodes. Easy!
      e => e match {
        case FlatMap(Filter(base: Exp[Traversable[_]], f: FuncExp[_, _ /*Boolean*/]), fmFun) =>
          val alphaRenamed = f.body.substSubTerm(f.x, fmFun.x) //tuplingTransform acts on var from flatMap functions, not filter functions.
          v += tuplingTransform(alphaRenamed.asInstanceOf[Exp[U]], fx).asInstanceOf[Exp[Boolean]] //XXX fix typing here
          OptimizationTransforms.stripView(base) flatMap fmFun
        case _ => e
      }
    }
    (res, v.fold(Const(true))(And))
  }

  private def groupByShareBodyNested[TupleT, U](indexBaseToLookup: Exp[Traversable[TupleT]],
                                      fx: TypedVar[TupleT],
                                      fEqBody: Eq[U],
                                      constantEqSide: Exp[U],
                                      varEqSide: Exp[U],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[TupleT]) => Exp[U]): Option[Exp[Traversable[TupleT]]] = {
    val varEqSideTransf = tuplingTransform(varEqSide, fx)

    //println("groupByShareBodyNested on " + indexBaseToLookup)
    val (baseNoFilter, filterCond) = withoutFilters(indexBaseToLookup, tuplingTransform, fx)
    val tries = Seq((indexBaseToLookup, Const(true)), (baseNoFilter, filterCond))
    collectFirst(tries) {
      case (base, cond) =>
        val groupedBy = base.groupBy[U](FuncExp.makefun[TupleT, U](varEqSideTransf, fx))

        assertType[Exp[U => Traversable[TupleT]]](groupedBy) //Just for documentation.
        val toLookup = Optimization.normalize(groupedBy)
        subqueries.get(toLookup) match {
          case Some(t) =>
            println("Found nested index of form " + toLookup)
            Some(asExp(t.asInstanceOf[Map[U, Traversable[TupleT]]]) get constantEqSide flatMap identity withFilter FuncExp.makefun(cond, fx))
          case None =>
            println("Found no nested index of form " + toLookup)
            None
        }
    }
  }

  /**
   *
   * @param indexBaseToLookup
   * @param allConds
   * @param fx
   * @param FVSeq
   * @param parentNode
   * @param parentF
   * @param isOption whether parentNode and the contained node have type Exp[ Option[_] ] rather than Exp[ Traversable[_] ]
   * @param cond one of allConds
   * @tparam TupleT
   * @tparam T
   * @return
   */
  private def tryGroupByNested[TupleT, T /*, U, V*/](indexBaseToLookup: Exp[Traversable[TupleT]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Var],
                            parentNode: Exp[Traversable[T]],
                            parentF: FuncExp[_ /*T*/, _/*U*/],
                            isOption: Boolean)
                           (cond: Eq[_]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[u] =>
        val allFVSeq = FVSeq :+ fx
        val allFVMap = allFVSeq.zipWithIndex.toMap
        val usesFVars = defUseFVars(allFVMap contains _) _
        def tuplingTransform[T, TupleT](e: Exp[T], tupleVar: TypedVar[TupleT]) = e.transform(
          exp => exp match {
            case v: Var if allFVMap contains v =>
              TupleSupport2.projectionTo(tupleVar, allFVSeq.length, allFVMap(v))
            case _ =>
              exp
          })

        //We never want to duplicate variables and take care of scoping.
        //However, we can reuse fx here while still getting a free variable in the end.
        val newVar = fx.asInstanceOf[TypedVar[TupleT]]
        //XXX probably this should get Exp[Traversable] or Exp[Option], depending on isOption.
        val step1Opt: Option[Exp[Traversable[TupleT]]] =
          if (usesFVars(eq.t1) && !usesFVars(eq.t2))
            groupByShareBodyNested[TupleT, u](indexBaseToLookup, newVar, eq, eq.t2, eq.t1, allFVSeq, tuplingTransform)
          else if (usesFVars(eq.t2) && !usesFVars(eq.t1))
            groupByShareBodyNested[TupleT, u](indexBaseToLookup, newVar, eq, eq.t1, eq.t2, allFVSeq, tuplingTransform)
          else None
        //We need to apply tuplingTransform both to allConds and to parentF.
        //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
        //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
        //tuplingTransform. Hence let's perform the desired alpha-conversion.
        val alphaRenamedParentF = parentF.body.substSubTerm(parentF.x, newVar)
        val step2Opt = step1Opt.map(e => residualQuery(e, (allConds - eq).map(tuplingTransform(_, newVar)), newVar))

        //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
        // instance from the HOAS representation produced.
        parentNode match {
          case FlatMap(_, _) =>
            step2Opt.map(e => e flatMap FuncExp.makefun[TupleT, Traversable[T]](
              tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[Traversable[T]]], newVar), newVar))
        }
      case _ => None
    }

  val groupByShareNested: Exp[_] => Exp[_] =
    e => {
      (for {
        ((parentNode: Exp[Traversable[t]], parentF), FoundFilter(c, f: FuncExp[_/*t*/, _ /*Boolean*/], isOption), conds, foundEqs, allFVSeq) <- lookupEq(None, e)
        optim <- {
            //Here we produce an open term, because vars in allFVSeq are not bound...
            def buildTuple(x: Exp[_]): Exp[_] = TupleSupport2.toTuple(allFVSeq :+ x)
            val indexQuery = c fold (
              cOpt  => (cOpt map buildTuple): Exp[Iterable[Any /*t*/]],
              cTrav => OptimizationTransforms.stripView(cTrav) map buildTuple)

            //... but here we replace parentNode with the open term we just constructed, so that vars in allFVSeq are
            //now bound. Note: e might well be an open term - we don't check it explicitly anywhere, even if we should.
            //However, index lookup is going to fail when open terms are passed - the query repository contains only
            //closed queries.
            //
            //Note: this means that we built the index we search by substitution in the original query; an alternative
            //approach would be to rebuild the index by completing indexQuery with the definitions of the open variables.
            val indexBaseToLookup = e.substSubTerm(parentNode, indexQuery).asInstanceOf[Exp[Traversable[Any]]]
            val optimized: Option[Exp[_]] = collectFirst(foundEqs)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode, parentF, isOption)(_))
            optimized
        }
      } yield optim).headOption.getOrElse(e)
    }
    */

  case class Equality[U](varEqSide: Exp[U], constantEqSide: Exp[U], orig: Eq[U])

  sealed abstract class FoundNode[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](val c: Exp[Repr with Traversable[T]]) {
    type OptRes
    def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[T]]],
                           parentNode: FlatMap[T, Repr, U, That],
                           allFVSeq: Seq[Var]): Option[Exp[Traversable[OptRes]]]
    type TupleWith[T]
    def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[TupleWith[T]]
  }
  case class FoundFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](override val c: Exp[Repr],
                                                              f: FuncExp[T, Boolean],
                                                              conds: Set[Exp[Boolean]],
                                                              foundEqs: Set[Equality[_]]) extends FoundNode[T, Repr](c) {
    type OptRes = T
    type TupleWith[T] = Any
    override def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[Any] = TupleSupport2.toTuple(allFVSeq :+ x)
    override def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[T]]],
                                   parentNode: FlatMap[T, Repr, U, That],
                                   allFVSeq: Seq[Var]) = collectFirst(foundEqs)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode, this)(_))
  }
  case class FoundTypeCase[BaseT,
  Repr <: Traversable[BaseT] with TraversableLike[BaseT, Repr],
  Res,
  That <: TraversableLike[Res, That]](t: TypeCaseExp[BaseT, Repr, Res, That]) extends FoundNode[BaseT, Repr](t.e) {
    type OptRes = Res
    type TupleWith[T] = (Any, T)
    override def buildTuple[T](allFVSeq: Seq[Var])(x: Exp[T]): Exp[(_, T)] = (TupleSupport2.toTuple(allFVSeq), x)
    override def optimize[TupleT, U, That <: Traversable[U]](indexBaseToLookup: Exp[Traversable[TupleWith[BaseT]]],
                                   parentNode: FlatMap[BaseT, Repr, U, That],
                                   allFVSeq: Seq[Var]): Option[Exp[Traversable[Res]]] = {
      (for (branch1 <- t.cases) yield {
        branch1 match {
          case branch2: TypeCase[t, _/*Res*/] =>
            val branch = branch2.asInstanceOf[TypeCase[t, Res]] //This cast should really not be needed, but Scalac can't infer it for some reason.
            val guard = branch.guard
            val conds = BooleanOperators.cnf(guard.body)
            //branch.f is also an open term which needs to be processed like the rest.
            //implicit val cm = ClassManifest fromClass branch.classS //XXX hack 1
            tryTypedGroupByNested(indexBaseToLookup, conds, guard.x, allFVSeq, parentNode, this)(branch.classS).map(_.asInstanceOf[Exp[Traversable[t]]] map branch.f)
          //collectFirst(conds)(tryGroupByNested(indexBaseToLookup, conds, guard.x, allFVSeq, parentNode, this)(_)).get
        }
      //}) reduce (_ ++ _)
      }) reduce {
        (opt1, opt2) =>
          for (a <- opt1; b <- opt2) yield a ++ b //Return something only if both Option values are defined.
          //_ ++ _
      }
    }
  }
  //case object FoundNothing extends FoundNode
  //case class FoundFlatMap[T, U](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Traversable[U]], isOption: Boolean = false) extends FoundNode

  def defUseFVars(fvContains: Var => Boolean)(e: Exp[_]) = e.findTotFun { case v: Var => fvContains(v); case _ => false }.nonEmpty

  def localLookupIndexableExps[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](e: FlatMap[T, Repr, U, That],
                          freeVars: Set[Var] = Set.empty,
                          fvSeq: Seq[Var] = Seq.empty): Seq[(FlatMap[T, Repr, U, That], FoundNode[T, Repr], Seq[Var])] = {
    e.base match {
      //case Filter(c: Exp[Traversable[T /*t*/]], f: FuncExp[_, _ /*Boolean*/]) =>
      case Filter(c, f) =>
        //case ff @ FoundFilter(_ /*: Exp[Traversable[_ /*t*/]]*/, f: FuncExp[t, _ /*Boolean*/])
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val allFreeVars: Set[Var] = freeVars + f.x
        val usesFVars = defUseFVars(allFreeVars) _

        //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
        //I managed, but it was beyond the abilities of type inference.
        val foundEqs =
          conds.map {
            case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty =>
              if (usesFVars(l) && !usesFVars(r))
                Seq(Equality(l, r, eq))
              else if (usesFVars(r) && !usesFVars(l))
                Seq(Equality(r, l, eq))
              else
                Seq.empty
            case _ => Seq.empty
          }.fold(Seq.empty)(_ union _).toSet[Equality[_]]
        //Seq((e, (ff, conds, foundEqs), fvSeq /*allFVSeq*/))
        Seq((e, FoundFilter[T, Repr](c, f, conds, foundEqs), fvSeq /*allFVSeq*/))
      case t1: TypeCaseExp[baseT, repr, res, that] =>
        val t = t1.asInstanceOf[TypeCaseExp[T, Repr, res, that]]
        Seq((e, FoundTypeCase(t), fvSeq))
      /*case t: TypeFilter[t, c, d, s] /*TypeFilter(base, f, classS)*/ =>
        //XXX: this builds expression nodes instead of using concrete syntax.
        Some(FoundTypeCase(TypeCaseExp(t.base.asInstanceOf[Exp[Traversable[d[t]]]],
          Seq(TypeCase[Any, Any](t.classS.asInstanceOf[Class[Any]], FuncExp((_: Any) => true), FuncExp(identity))))))
        //when[s](identity))))*/
      case _ => Seq.empty
    }
  }

  def lookupIndexableExps(e: Exp[_],
               freeVars: Set[Var] = Set.empty,
               fvSeq: Seq[Var] = Seq.empty): Seq[(FlatMap[_, _, _, _], FoundNode[_, _], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)

    val otherRes = e match {
      case f: FuncExp[_, _] =>
        lookupIndexableExps(f.body, freeVars + f.x, fvSeq :+ f.x)
      case exp => exp.children flatMap {
        lookupIndexableExps(_, freeVars, fvSeq)
      }
    }
    otherRes ++ (e match {
      case f: FlatMap[t, repr, u, that] =>
      //case FlatMap(c: Exp[Traversable[t]], _: FuncExp[_ /*t*/, Traversable[u]]) =>
        localLookupIndexableExps(f, freeVars, fvSeq)
      case _ => Seq.empty
    })
  }

  /*
   * When looking up an index, a filter in the original query will be preserved in the lookup key, and it might make the
   * lookup fail. If an index exists without a filter, we should try to use it.
   * Some filters might be required to prevent subsequent operations from failing, hence they might be required also in
   * the index; stripping the filter will thus create a query which would fail at runtime.
   * That is not a problem, since we don't execute the queries we build, just look them up; if the lookup is successful,
   * it means we constructed a query which the user added to the precomputed query repository, hence it better be safe.
   * An actual concern is that we might need to strip only some indexes but not others from the query in order to find a
   * matching index.
   */
  private def withoutFilters[TupleT, U](indexBaseToLookup: Exp[Traversable[TupleT]], tuplingTransform: (Exp[U], TypedVar[TupleT]) => Exp[U], fx: TypedVar[TupleT]): (Exp[Traversable[TupleT]], Exp[Boolean]) = {
    val v = ArrayBuffer[Exp[Boolean]]()
    //We are removing too many filters, and not keeping track of their bindings :-(.
    val res = indexBaseToLookup transform { //We only want to recognize filtering in the collection branch of FlatMap nodes. Easy!
      e => e match {
        case FlatMap(Filter(base: Exp[Traversable[_]], f: FuncExp[_, _ /*Boolean*/]), fmFun) =>
          val alphaRenamed = f.body.substSubTerm(f.x, fmFun.x) //tuplingTransform acts on var from flatMap functions, not filter functions.
          v += tuplingTransform(alphaRenamed.asInstanceOf[Exp[U]], fx).asInstanceOf[Exp[Boolean]] //XXX fix typing here
          OptimizationTransforms.stripView(base) flatMap fmFun
        case _ => e
      }
    }
    (res, v.fold(Const(true))(And))
  }

  private def groupByShareBodyNested[TupleT, U](indexBaseToLookup: Exp[Traversable[TupleT]],
                                      fx: TypedVar[TupleT],
                                      fEqBody: Equality[U],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[TupleT]) => Exp[U]): Option[Exp[Traversable[TupleT]]] = {
    import fEqBody.{varEqSide, constantEqSide}
    val varEqSideTransf = tuplingTransform(varEqSide, fx)

    //println("groupByShareBodyNested on " + indexBaseToLookup)
    val (baseNoFilter, filterCond) = withoutFilters(indexBaseToLookup, tuplingTransform, fx)
    val tries = Seq((indexBaseToLookup, Const(true)), (baseNoFilter, filterCond))
    collectFirst(tries) {
      case (base, cond) =>
        val groupedBy = base.groupBy[U](FuncExp.makefun[TupleT, U](varEqSideTransf, fx))

        assertType[Exp[U => Traversable[TupleT]]](groupedBy) //Just for documentation.
        val toLookup = Optimization.normalize(groupedBy)
        subqueries.get(toLookup) match {
          case Some(t) =>
            println("Found nested index of form " + toLookup)
            Some(asExp(t.asInstanceOf[Map[U, Traversable[TupleT]]]) get constantEqSide flatMap identity withFilter FuncExp.makefun(cond, fx))
          case None =>
            println("Found no nested index of form " + toLookup)
            None
        }
    }
  }

  /**
   *
   * @param indexBaseToLookup
   * @param allConds
   * @param fx
   * @param FVSeq
   * @param parentNode
   * @param eq one of allConds
   * @tparam TupleT
   * @tparam T
   * @return
   */
  private def tryGroupByNested[TupleT, U, FmT, FmRepr <: Traversable[FmT] with TraversableLike[FmT, FmRepr], FmU, FmThat <: Traversable[FmU]](indexBaseToLookup: Exp[Traversable[TupleT]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Var],
                            parentNode: FlatMap[FmT, FmRepr, FmU, FmThat], fn: FoundNode[FmT, FmRepr])
                           (eq: Equality[U]): Option[Exp[Traversable[FmT]]] = {
    val allFVSeq = FVSeq :+ fx
    val allFVMap = allFVSeq.zipWithIndex.toMap
    val usesFVars = defUseFVars(allFVMap contains _) _
    //Filter-specific
    assert(usesFVars(eq.varEqSide) && !usesFVars(eq.constantEqSide))
    def tuplingTransform[T, TupleT](e: Exp[T], tupleVar: TypedVar[TupleT]) = e.transform(
      exp => exp match {
        case v: Var if allFVMap contains v =>
          TupleSupport2.projectionTo(tupleVar, allFVSeq.length, allFVMap(v))
        case _ =>
          exp
      })

    //We never want to duplicate variables and take care of scoping.
    //However, we can reuse fx here while still getting a free variable in the end.
    val newVar = fx.asInstanceOf[TypedVar[TupleT]]
    //Filter-specific
    val step1Opt: Option[Exp[Traversable[TupleT]]] =
      groupByShareBodyNested[TupleT, U](indexBaseToLookup, newVar, eq, allFVSeq, tuplingTransform)
    //We need to apply tuplingTransform both to allConds and to parentF.
    //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
    //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
    //tuplingTransform. Hence let's perform the desired alpha-conversion.
    val alphaRenamedParentF = parentNode.f.body.substSubTerm(parentNode.f.x, newVar)
    //residualQuery is also filter-specific, in this form. TypeCase however has guards.
    val step2Opt = step1Opt.map(e => residualQuery(e, (allConds - eq.orig).map(tuplingTransform(_, newVar)), newVar))

    //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
    // instance from the HOAS representation produced.
    parentNode match {
      case FlatMap(_, _) =>
        step2Opt.map(e => e flatMap FuncExp.makefun[TupleT, Traversable[FmT]](
          tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[Traversable[FmT]]], newVar), newVar))
    }
  }

  private def typedGroupByShareBodyNested[TupleT, T /*: ClassManifest*/, U](indexBaseToLookup: Exp[Traversable[(TupleT, T)]],
                                      fx: TypedVar[(TupleT, T)],
                                      clazz: Class[_],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[(TupleT, T)]) => Exp[U]): Option[Exp[Traversable[(TupleT, T)]]] = {
    //import fEqBody.{varEqSide, constantEqSide}
    //val varEqSideTransf = tuplingTransform(varEqSide, fx)

    //println("groupByShareBodyNested on " + indexBaseToLookup)
    val (baseNoFilter, filterCond) = withoutFilters(indexBaseToLookup, tuplingTransform, fx)
    val tries = Seq((indexBaseToLookup, Const(true)), (baseNoFilter, filterCond))
    collectFirst(tries) {
      case (base, cond) =>
        //val groupedBy = base.groupBy[U](FuncExp.makefun[TupleT, U](varEqSideTransf, fx))
        val groupedBy = base.asInstanceOf[Exp[Traversable[(TupleT, Any /*T*/)]]].groupByTupleType2 //XXX hack 2 - use of Any to accept a fixed manifest,
        // instead of classManifest[T] which is unfortunately not available. The use of the manifest however is just a
        // small (and premature?) optimization.

        //assertType[Exp[U => Traversable[TupleT]]](groupedBy) //Just for documentation.
        val toLookup = Optimization.normalize(groupedBy)
        subqueries.get(toLookup) match {
          case Some(t) =>
            println("Found nested type-index of form " + toLookup)
            type TupleTAnd[+T] = (TupleT, T)
            //This map step is wrong, we rely on substitution!
            Some(asExp(t.asInstanceOf[TypeMapping[Traversable, TupleTAnd, AnyRef]]).get[T](clazz) /*map(_._1) */withFilter FuncExp.makefun(cond, fx))
          case None =>
            println("Found no nested type-index of form " + toLookup)
            None
        }
    }
  }

  private def tryTypedGroupByNested[TupleT, U, FmT /*: ClassManifest*/, FmRepr <: Traversable[FmT] with TraversableLike[FmT, FmRepr], FmU, FmThat <: Traversable[FmU]](indexBaseToLookup: Exp[Traversable[(TupleT, FmT)]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Var],
                            parentNode: FlatMap[FmT, FmRepr, FmU, FmThat], fn: FoundNode[FmT, FmRepr])
                           (clazz: Class[_]): Option[Exp[Traversable[FmT]]] = {
    val allFVSeq = FVSeq :+ fx
    val FVMap = FVSeq.zipWithIndex.toMap
    //val usesFVars = defUseFVars(allFVMap contains _) _
    def tuplingTransform[T, U, TupleT](e: Exp[T], tupleVar: TypedVar[(TupleT, U)]) = e.transform(
      exp => exp match {
        case v: Var if v == fx || (FVMap contains v) =>
          if (v == fx)
            tupleVar._2
          else
            //TupleSupport2.projectionTo(tupleVar._1, allFVSeq.length, FVMap(v)) //now this is incorrect!
            TupleSupport2.projectionTo(tupleVar, FVSeq.length, FVMap(v)).substSubTerm(tupleVar, tupleVar._1)
        case _ =>
          exp
      })

    //We never want to duplicate variables and take care of scoping.
    //However, we can reuse fx here while still getting a free variable in the end.
    val newVar = fx.asInstanceOf[TypedVar[(TupleT, FmT)]]
    //Filter-specific
    val step1Opt: Option[Exp[Traversable[(TupleT, FmT)]]] =
      typedGroupByShareBodyNested[TupleT, FmT, U](indexBaseToLookup, newVar, clazz, allFVSeq, tuplingTransform)
    //We need to apply tuplingTransform both to allConds and to parentF.
    //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
    //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
    //tuplingTransform. Hence let's perform the desired alpha-conversion.
    val alphaRenamedParentF = parentNode.f.body.substSubTerm(parentNode.f.x, newVar)
    //residualQuery is also filter-specific, in this form. TypeCase however has guards.
    val step2Opt = step1Opt.map(e => residualQuery(e, (allConds/* - eq.orig*/).map(tuplingTransform(_, newVar)), newVar))

    //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
    // instance from the HOAS representation produced.
    parentNode match {
      case FlatMap(_, _) =>
        step2Opt.map(e => e flatMap FuncExp.makefun[(TupleT, FmT), Traversable[FmT]](
          tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[Traversable[FmT]]], newVar), newVar))
    }
  }

  val groupByShareNested: Exp[_] => Exp[_] =
    {
      case e: FuncExp[_, _] => e
      case e =>
      (for {
        ((parentNode: FlatMap[t, repr, u, that/*T, Repr, U, That*/]), fn1: FoundNode[_, _], allFVSeq) <- lookupIndexableExps(e)
      } yield {
        val fn = fn1.asInstanceOf[FoundNode[t, repr]]
        //buildTuple produces an open term, because vars in allFVSeq are not bound...
        val indexQuery = OptimizationTransforms.stripView(fn.c) map fn.buildTuple(allFVSeq)
        //... but here we replace parentNode with the open term we just constructed, so that vars in allFVSeq are
        //now bound. Note: e might well be an open term - we don't check it explicitly anywhere, even if we should.
        //However, index lookup is going to fail when open terms are passed - the query repository contains only
        //closed queries.
        //
        //Note: this means that we built the index we search by substitution in the original query; an alternative
        //approach would be to rebuild the index by completing indexQuery with the definitions of the open variables.
        val indexBaseToLookup = e.substSubTerm(parentNode, indexQuery).asInstanceOf[Exp[Traversable[fn.TupleWith[t]]]]

        if (indexBaseToLookup.freeVars == Set.empty)
          fn.optimize(indexBaseToLookup, parentNode, allFVSeq).asInstanceOf[Option[Exp[Traversable[_]]]]
        else {
          println("Stopping index search because of open term indexBaseToLookup = " + indexBaseToLookup)
          None
        }
      }).headOption flatMap identity getOrElse e
    }

  //Entry point
  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
    println("Index lookup on query: " + query)
    query.transform(directsubqueryShare andThen groupByShare andThen groupByShareNested)
  }
}
