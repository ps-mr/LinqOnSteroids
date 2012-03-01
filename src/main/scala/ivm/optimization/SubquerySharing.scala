package ivm.optimization

import ivm.expressiontree._
import Lifting._
import Util.assertType
import scala.collection.Map

// Contract: Each map entry has the form Exp[T] -> T for some T
/**
 * Note: this code is not allowed to create expression nodes through their constructors, as with {@see Optimization}.
 */
class SubquerySharing(val subqueries: Map[Exp[_], Any]) {
  val directsubqueryShare: Exp[_] => Exp[_] = {
    e => subqueries.get(Optimization.normalize(e)) match {
      case Some(t) => asExp(t)
      case None => e
    }
  }

  private def residualQuery[T](e: Exp[Traversable[T]], conds: Set[Exp[Boolean]], v: TypedVar[_ /*T*/]): Exp[Traversable[T]] = {
    //This special case is an optimization which should not be needed. We need other optimizations to remove the extra resulting stuff.
    if (conds.isEmpty)
      return e
    val residualcond: Exp[Boolean] = conds.reduce(And)
    //Note that withFilter will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
    // instance from the HOAS representation produced.
    e withFilter FuncExp.makefun[T, Boolean](residualcond, v)
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

  //This is equivalent to coll.collectFirst(Function.unlift(f)), but it saves the expensive Function.unlift.
  private def collectFirst[T, U](coll: TraversableOnce[T])(f: T => Option[U]): Option[U] = {
    for (x <- coll) {
      f(x) match {
        case v@Some(_) => return v
        case _ =>
      }
    }
    None
  }

  //We have to strip View if needed on _both_ sides before performing the match, to increase the chance of a match.
  //This is done here on one side, and on Optimization.addSubquery on the other side. Note that only the top-level strip
  //is visible.
  //Rewrite (if possible) coll.withFilter(elem => F[elem] === k && OtherConds[elem]) to (coll.groupBy(elem => F[elem]))(k).withFilter(x => OtherConds[x]),
  //with F and OtherConds expression contexts and under the condition that coll.groupBy(f) is already available as a precomputed subquery (i.e. an index).
  val groupByShare: Exp[_] => Exp[_] = {
    e => e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupBy(OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]]), conds, f.x)(_))
        optimized.getOrElse(e)
      case _ => e
    }
  }

  sealed trait FoundNode
  case class FoundFilter[T](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, Boolean], isOption: Boolean = false) extends FoundNode
  case class FoundFlatMap[T, U](c: Either[Exp[Option[T]], Exp[Traversable[T]]], f: FuncExp[T, TraversableOnce[U]], isOption: Boolean = false) extends FoundNode

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
               fvSeq: Seq[Var] = Seq.empty): Seq[(Option[(Exp[Traversable[_]], FuncExp[_, _])], FoundFilter[_], Set[Exp[Boolean]], Set[Exp[_]], Seq[Var])] = {
    require (fvSeq.toSet == freeVars)
    import OptionOps._

    val matchResult: Either[Exp[_], FoundNode] = e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) if parent.isDefined =>
        Right(FoundFilter(Right(c), f))
      case Call2(OptionFilterId, _, subColl: Exp[Option[t]], f: FuncExp[_, _]) if parent.isDefined =>
        //XXX not so sure we want to optimize such a one-element filter.
        //But it can be a one-element filter on top of various navigation operations, so it can still make sense.
        Right(FoundFilter(Left(subColl), f.asInstanceOf[FuncExp[t, Boolean]], true))
      case FlatMap(c: Exp[Traversable[_]], f: FuncExp[t, TraversableOnce[u]]) =>
        Right(FoundFlatMap(Right(c), f))
      case Call2(OptionFlatMapId, _, subColl: Exp[Option[_]], f: FuncExp[t, TraversableOnce[u]]) =>
        Right(FoundFlatMap(Left(subColl.asInstanceOf[Exp[Option[t]]]), f, true))
      case _ =>
        Left(e)
    }

    matchResult.fold(
      _ match {
        case f: FuncExp[_, _] =>
          lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case exp => exp.children flatMap {
          lookupEq(None, _, freeVars, fvSeq)
        }
      },
      _ match {
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
            }.fold(Seq.empty)(_ union _).toSet[Exp[_]]
          Seq((parent, ff, conds, foundEqs, fvSeq /*allFVSeq*/)) //Don't include the variable of the filter, which is going to be dropped anyway - hence fvSeq, not allFVSeq
        //case FlatMap(c, f: FuncExp[t, Traversable[u]]) =>
        //lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c, freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
        case FoundFlatMap(c, f: FuncExp[t, Traversable[u]], _) =>
          //Add all free variables bound in the location.
          lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c.fold(identity, identity), freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
      })
  }

  private def groupByShareBodyNested[TupleT, T, U](indexBaseToLookup: Exp[Traversable[TupleT]],
                                      fx: TypedVar[Seq[T]],
                                      fEqBody: Eq[U],
                                      constantEqSide: Exp[U],
                                      varEqSide: Exp[U],
                                      allFVSeq: Seq[Var],
                                      tuplingTransform: (Exp[U], TypedVar[Seq[T]]) => Exp[U]): Option[Exp[Traversable[TupleT]]] = {
    val varEqSideTransf = tuplingTransform(varEqSide, fx)
    val groupedBy = indexBaseToLookup.groupBy[U](FuncExp.makefun[TupleT, U](varEqSideTransf, fx))

    assertType[Exp[U => Traversable[TupleT]]](groupedBy) //Just for documentation.
    val toLookup = Optimization.normalize(groupedBy)
    subqueries.get(toLookup) match {
      case Some(t) =>
        println("Found nested index of form " + toLookup)
        Some(asExp(t.asInstanceOf[Map[U, Traversable[TupleT]]]) get constantEqSide flatMap identity)
      case None =>
        println("Found no nested index of form " + toLookup)
        None
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
                           (cond: Exp[Boolean]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[u] =>
        val allFVSeq = FVSeq :+ fx
        val allFVMap = allFVSeq.zipWithIndex.toMap
        val usesFVars = defUseFVars(allFVMap contains _) _
        def tuplingTransform[T, U](e: Exp[T], tupleVar: TypedVar[Seq[U]]) = e.transform(
          exp => exp match {
            case v: Var if allFVMap contains v =>
              TupleSupport2.projectionTo(tupleVar, allFVSeq.length, allFVMap(v))
            case _ =>
              exp
          })

        //We never want to duplicate variables and take care of scoping.
        //However, we can reuse fx here while still getting a free variable in the end.
        val newVar = fx.asInstanceOf[TypedVar[Seq[T]]]
        //XXX probably this should get Exp[Traversable] or Exp[Option], depending on isOption.
        val step1Opt: Option[Exp[Traversable[TupleT]]] =
          if (usesFVars(eq.t1) && !usesFVars(eq.t2))
            groupByShareBodyNested[TupleT, T, u](indexBaseToLookup, newVar, eq, eq.t2, eq.t1, allFVSeq, tuplingTransform)
          else if (usesFVars(eq.t2) && !usesFVars(eq.t1))
            groupByShareBodyNested[TupleT, T, u](indexBaseToLookup, newVar, eq, eq.t1, eq.t2, allFVSeq, tuplingTransform)
          else None
        //We need to apply tuplingTransform both to allConds and to parentF.
        //About parentF, note that fx is _not_ in scope in its body, which uses another variable, which
        //iterates over the same collection as fx, so it should be considered equivalent to fx from the point of view of
        //tuplingTransform. Hence let's perform the desired alpha-conversion.
        val alphaRenamedParentF = parentF.body.substSubTerm(parentF.x, newVar)
        val step2Opt = step1Opt.map(e => residualQuery(e, (allConds - eq).map(tuplingTransform(_, newVar)), newVar))

        //Note that here, map/flatMap will ensure to use a fresh variable for the FuncExp to build, since it builds a FuncExp
        // instance from the HOAS representation produced.
        import OptionOps._
        parentNode match {
          case Call2(OptionFlatMapId, _, _, _) | FlatMap(_, _) =>
            step2Opt.map(e => e flatMap FuncExp.makefun[TupleT, TraversableOnce[T]](
              tuplingTransform(alphaRenamedParentF.asInstanceOf[Exp[TraversableOnce[T]]], newVar), newVar))
        }
      case _ => None
    }

  val groupByShareNested: Exp[_] => Exp[_] = {
    e => {
      (for {
        (Some((parentNode: Exp[Traversable[t]], parentF)), filterExp, conds, foundEqs, allFVSeq) <- lookupEq(None, e)
        optim <- filterExp match {
          case FoundFilter(c, f: FuncExp[t, _ /*Boolean*/], isOption) =>
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
            val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupByNested(indexBaseToLookup, conds, f.x, allFVSeq, parentNode, parentF, isOption)(_))
            optimized
          //case _ => e //Execution must not get here - hence, throw an exception
        }
      } yield optim).headOption.getOrElse(e)
    }
  }

  //Entry point
  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
    println("Index lookup on query: " + query)
    query.transform(directsubqueryShare andThen groupByShare andThen groupByShareNested)
  }
}
