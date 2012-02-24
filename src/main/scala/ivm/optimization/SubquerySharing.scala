package ivm.optimization

import ivm.expressiontree._
import Lifting._
import Util.assertType
import scala.collection.Map

// Contract: Each map entry has the form Exp[T] -> T for some T
class SubquerySharing(val subqueries: Map[Exp[_], Any]) {
  val directsubqueryShare: Exp[_] => Exp[_] = {
    e => subqueries.get(Optimization.normalize(e)) match {
      case Some(t) => Const(t)
      case None => e
    }
  }

  private def residualQuery[T](e: Exp[Traversable[T]], conds: Set[Exp[Boolean]], v: TypedVar[_ /*T*/]): Exp[Traversable[T]] = {
    //This special case is an optimization which should not be needed. We need other optimizations to remove the extra resulting stuff.
    if (conds.isEmpty)
      return e
    val residualcond: Exp[Boolean] = conds.reduce(And)
    e.withFilter(FuncExp.makefun[T, Boolean](residualcond, v))
  }

  private def groupByShareBody[T, T2](c: Exp[Traversable[T]],
                                      fx: Var,
                                      fEqBody: Eq[T2], constantEqSide: Exp[T2], varEqSide: Exp[T2]) = {
    val groupedBy = c.groupBy[T2](FuncExp.makefun[T, T2](varEqSide, fx).f)

    assertType[Exp[T2 => Traversable[T]]](groupedBy) //Just for documentation.
    subqueries.get(Optimization.normalize(groupedBy)) match {
      case Some(t) => Some(asExp(t.asInstanceOf[T2 => Traversable[T]]).apply(constantEqSide))
      case None => None
    }
  }

  private def tryGroupBy[T](c: Exp[Traversable[T]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var)
                           (cond: Exp[Boolean]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[t2] =>
        val oq: Option[Exp[Traversable[T]]] =
          if (eq.t1.isOrContains(fx) && !eq.t2.isOrContains(fx))
            groupByShareBody[T, t2](c, fx, eq, eq.t2, eq.t1)
          else if (eq.t2.isOrContains(fx) && !eq.t1.isOrContains(fx))
            groupByShareBody[T, t2](c, fx, eq, eq.t1, eq.t2)
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
  //This is done here on one side, and on Optimization.addSubQuery on the other side. Note that only the top-level strip
  //is visible.
  //Rewrite (if possible) coll.withFilter(elem => F[elem] === k && OtherConds[elem]) to (coll.groupBy(elem => F[elem]))(k).withFilter(x => OtherConds[x]),
  //with F and OtherConds expression contexts and under the condition that coll.groupBy(f) is already available as a precomputed subquery (i.e. an index).
  val groupByShare: Exp[_] => Exp[_] = {
    e => e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupBy(OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]]), conds, f.x.asInstanceOf[Var])(_))
        optimized.getOrElse(e)
      case _ => e
    }
  }

  //Preconditions:
  //Postconditions: when extracting Some(parent), filterExp, foundEqs, allFVSeq, parent is a MapOp/FlatMap node which
  // contains filterExp as child as base collection (and not through a FuncExp node).
  def lookupEq(parent: Option[(Exp[Traversable[_]], FuncExp[_, _])],
               e: Exp[_],
               freeVars: Set[Exp[_]] = Set.empty,
               fvSeq: Seq[Exp[_]] = Seq.empty): Seq[(Option[(Exp[Traversable[_]], FuncExp[_, _])], Exp[_], Set[Exp[_]], Seq[Exp[_]])] = {
    require (fvSeq.toSet == freeVars)
    e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) if parent.isDefined =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val allFreeVars = freeVars + f.x
        def usesFVars(e: Exp[_]) = e.findTotFun(allFreeVars(_)).nonEmpty

        //Using Sets here directly is very difficult, due to the number of wildcards and the invariance of Set.
        //I managed, but it was beyond the abilities of type inference.
        val foundEqs =
          conds.map {
            case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesFVars(l) && !usesFVars(r) || usesFVars(r) && !usesFVars(l)) =>
              Seq(eq)
            case _ => Seq.empty
          }.fold(Seq.empty)(_ union _).toSet[Exp[_]]
        Seq((parent, e, foundEqs, fvSeq /*allFVSeq*/)) //Don't include the variable of the filter, which is going to be dropped anyway.
      case FlatMap(c, f: FuncExp[t, u]) =>
        //Add all free variables bound in the location.
        lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c, freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
      case MapOp(c, f: FuncExp[t, u]) =>
        lookupEq(Some((e.asInstanceOf[Exp[Traversable[u]]], f)), c, freeVars, fvSeq) union lookupEq(None, f.body, freeVars + f.x, fvSeq :+ f.x)
      case _ => Seq.empty
    }
  }

  private def groupByShareBodyNested[T, U](indexBaseToLookup: Exp[Traversable[Seq[T]]],
                                      fx: Var,
                                      fEqBody: Eq[U],
                                      constantEqSide: Exp[U], 
                                      varEqSide: Exp[U],
                                      allFVSeq: Seq[Exp[_]],
                                      parentNode: Exp[_/*T*/], parentF: FuncExp[_, _/*T, U*/],
                                      tuplingTransform: (Exp[U], TypedVar[Seq[T]]) => Exp[U]): Option[Exp[Traversable[Seq[T]]]] = {
    //val fxIdx = allFVSeq.indexOf(fx)
    //val tupleV = FuncExp.gensym
    val varEqSideTransf = tuplingTransform(varEqSide, fx.asInstanceOf[TypedVar[Seq[T]]]).asInstanceOf[Exp[U]]
    val groupedBy = indexBaseToLookup.groupBy[U](FuncExp.makefun[Seq[T], U](varEqSideTransf, fx).f)

    assertType[Exp[U => Traversable[Seq[T]]]](groupedBy) //Just for documentation.
    subqueries.get(Optimization.normalize(groupedBy)) match {
      case Some(t) => Some(App(Const(t.asInstanceOf[U => Traversable[Seq[T]]]), constantEqSide))
      case None => None
    }
  }

  private def tryGroupByNested[T /*, U, V*/](indexBaseToLookup: Exp[Traversable[Seq[T]]],
                            allConds: Set[Exp[Boolean]],
                            fx: Var, FVSeq: Seq[Exp[_]], parentNode: Exp[Traversable[T]], parentF: FuncExp[_ /*T*/, _/*U*/])
                           (cond: Exp[Boolean]): Option[Exp[Traversable[T]]] =
    cond match {
      case eq: Eq[t2] =>
        val allFVSeq = FVSeq :+ fx
        val allFVMap = allFVSeq.zipWithIndex.toMap
        //val allFreeVars = allFVSeq.toSet
        def usesFVars(e: Exp[_]) = e.findTotFun(allFVMap.contains(_)).nonEmpty
        def tuplingTransform[T, U](e: Exp[T], v: TypedVar[Seq[U]]) = e.transform(
          e => allFVMap.get(e) match {
            case Some(idx) => v(idx)
            case None => e
          })

        val oq: Option[Exp[Traversable[Seq[T]]]] =
          if (usesFVars(eq.t1) && !usesFVars(eq.t2))
            groupByShareBodyNested[T, t2](indexBaseToLookup, fx, eq, eq.t2, eq.t1, allFVSeq, parentNode, parentF, tuplingTransform)
          else if (usesFVars(eq.t2) && !usesFVars(eq.t1))
            groupByShareBodyNested[T, t2](indexBaseToLookup, fx, eq, eq.t1, eq.t2, allFVSeq, parentNode, parentF, tuplingTransform)
          else None
        //We need replacements both in allConds and in parentF
        //First transform parentF. Note that fx is _not_ in scope in its body, but it uses another variable; we need it to use
        //the same variable for our purposes.
        //XXX we should use gensym, not duplicate vars!
        val newVar = fx.asInstanceOf[TypedVar[Seq[Any]]]
        val transfParentF = parentF.body.substSubTerm(parentF.x, newVar)
        val step1 = oq.map(e => residualQuery(e, (allConds - eq).map(tuplingTransform(_, newVar)), newVar))
        
        parentNode match {
          case MapOp(_, _) =>
            step1.map(e => e map FuncExp.makefun[Seq[T], T](tuplingTransform(transfParentF.asInstanceOf[Exp[T]], newVar), newVar).f)
          case FlatMap(_, _) =>
            step1.map(e => e flatMap FuncExp.makefun[Seq[T], TraversableOnce[T]](tuplingTransform(transfParentF.asInstanceOf[Exp[TraversableOnce[T]]], newVar), newVar).f)
        }
          //Note that we use always _map_ inside!
      case _ => None
    }

  val groupByShareNested: Exp[_] => Exp[_] = {
    e => {
      (for {
        (Some((parentNode: Exp[Traversable[t]], parentF)), filterExp, foundEqs, allFVSeq) <- lookupEq(None, e)
      } yield {
        //if (foundEqs.nonEmpty) {}
        filterExp match {
          case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
            val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
            val replacementBase = OptimizationTransforms.stripViewUntyped(c).asInstanceOf[Exp[Traversable[t]]]
            val indexQuery = replacementBase map (x => Seq(allFVSeq :+ x: _*))

            val indexBaseToLookup = e.substSubTerm(parentNode, indexQuery).asInstanceOf[Exp[Traversable[Seq[Any]]]]
            val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupByNested(indexBaseToLookup, conds, f.x.asInstanceOf[Var], allFVSeq, parentNode, parentF)(_))
            optimized.getOrElse(e)
          //case _ => e //Unneeded
        }
      }).headOption.getOrElse(e)
    }
  }

  //Entry point
  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
    query.transform(directsubqueryShare.andThen(groupByShare).andThen(groupByShareNested))
  }
}
