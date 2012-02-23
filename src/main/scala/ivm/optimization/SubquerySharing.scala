package ivm.optimization

import ivm.expressiontree._
import Lifting._
import Util.assertType
import collection.generic.FilterMonadic
import scala.collection.Map

// Contract: Each map entry has the form Exp[T] -> T for some T
class SubquerySharing(val subqueries: Map[Exp[_], Any]) {
  val directsubqueryShare: Exp[_] => Exp[_] = {
    e => subqueries.get(Optimization.normalize(e)) match {
      case Some(t) => Const(t)
      case None => e
    }
  }

  private def groupByShareBody[T, T2](c: Exp[Traversable[T]],
                                      f: FuncExp[T, Boolean],
                                      fEqBody: Eq[T2], constantEqSide: Exp[T2], varEqSide: Exp[T2]) = {
    /*
    //How the heck does this typecheck?
    val groupedBy2: Exp[T => Traversable[T]] = c.map(identity).groupBy(FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
    //Reason: a FuncExp[T, T2] is also FuncExp[T, Any]; groupBy(f: FuncExp[T, Any]) gives Map[Any, Repr]
    // (where Repr = Traversable[T] here) which can be _upcast_ to Any => Repr and then further upcast to T => Repr.
    // It's an interesting anomaly of type inference with variance, but it does not produce unsoundness.
    // Indeed, since Map[A, +B] is invariant in A, we can't upcast Map[Any, Repr] to Map[T, Repr] - see groupedBy5.
    //val groupedBy3: Exp[T => Traversable[T]] = c.map(identity).groupBy[T2](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
    val groupedBy4: Exp[T => Traversable[T]] = c.map(identity).groupBy[Any](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
    //
    val groupedBy5: Exp[Map[Any, Traversable[T]]] = c.map(identity).groupBy[Any](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
    */
    //This code:
    //val groupedBy = c.map(identity).groupBy(FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
    //expands to this:

    val groupedBy = c.groupBy[T2](FuncExp.makefun[T, T2](varEqSide, f.x).f)

    assertType[Exp[T2 => Traversable[T]]](groupedBy) //Just for documentation.
    subqueries.get(Optimization.normalize(groupedBy)) match {
      case Some(t) => Some(App(Const(t.asInstanceOf[T2 => Traversable[T]]), constantEqSide))
      case None => None
    }
  }

  private def residualQuery[T](e: Exp[Traversable[T]], conds: Set[Exp[Boolean]], v: TypedVar[_ /*T*/]): Exp[FilterMonadic[T, Traversable[T]]] = {
    //This special case is an optimization which should not be needed. We need other optimizations to remove the extra resulting stuff.
    if (conds.isEmpty)
      return e
    val residualcond: Exp[Boolean] = conds.reduce(And)
    e.withFilter(FuncExp.makefun[T, Boolean](residualcond, v))
  }

  private def tryGroupBy[T](c: Exp[Traversable[T]],
                            allConds: Set[Exp[Boolean]],
                            f: FuncExp[T, Boolean])
                           (cond: Exp[Boolean]): Option[Exp[FilterMonadic[T, Traversable[T]]]] =
    cond match {
      case eq: Eq[t2] =>
        val oq: Option[Exp[Traversable[T]]] =
          if (eq.t1.isOrContains(f.x) && !eq.t2.isOrContains(f.x))
            groupByShareBody[T, t2](c, f, eq, eq.t2, eq.t1)
          else if (eq.t2.isOrContains(f.x) && !eq.t1.isOrContains(f.x))
            groupByShareBody[T, t2](c, f, eq, eq.t1, eq.t2)
          else None
        oq.map(e => residualQuery(e, allConds - eq, f.x))
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
  val groupByShare2: Exp[_] => Exp[_] = {
    e => e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupBy(OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]]), conds, f)(_))
        optimized.getOrElse(e)
      case _ => e
    }
  }

  def par[A, B, C](f: (A, B) => C): ((A, B), (A, B)) => (C, C) = { case ((a1, b1), (a2, b2)) => (f(a1, b1), f(a2, b2))}

  //Next step: collect free variables relevant to the query... maybe...
  def lookupEq(e: Exp[_], freeVars: Set[Exp[_]]): Set[Exp[_]] = {
    e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
        val allFreeVars = freeVars + f.x
        def usesVar(e: Exp[_]) = e.findTotFun(allFreeVars(_)).nonEmpty

        /*def unionSeq[T] = (_: Seq[T]) union (_: Seq[T])

        conds.map {
          case eq @ Eq(l, r) => (((l.find {case Var(_) => true}) union r.find {case Var(_) => true}).toSet, Set[Exp[_]](eq))
          case _ => Set.empty[Exp[_]], Set.empty[Exp[_]])
        }.fold((Set.empty[Exp[_]], Set.empty[Exp[_]]))(par(unionSet))

        conds.map {
          case eq @ Eq(l, r) => (l.find {case Var(_) => true} union r.find {case Var(_) => true}, Seq[Exp[_]](eq))
          case _ => (Seq.empty, Seq.empty)
        }.fold((Seq.empty, Seq.empty))(par(unionSeq))*/

        //Using Sets here directly is close to impossible, due to the number of wildcards and the invariance of Set.
        conds.map {
          case eq @ Eq(l, r) if (eq find {case Var(_) => true}).nonEmpty && (usesVar(l) && !usesVar(r) || usesVar(r) && !usesVar(l)) =>
            Set[Exp[_]](eq)
          case _ => Set.empty[Exp[_]]
        }.fold(Set.empty[Exp[_]])(_ union _)
      case FlatMap(c, f) =>
        //Add to this the variables on which the free vars of subexp depend? No. Add all free variables bound in the location.
        lookupEq(c, freeVars) union lookupEq(f.body, freeVars + f.x)
      case MapOp(c, f) =>
        lookupEq(c, freeVars) union lookupEq(f.body, freeVars + f.x)
      case _ => Set.empty
    }
  }

  val groupByShare3: Exp[_] => Exp[_] = {
    e => {
      if (lookupEq(e, Set.empty).nonEmpty) {}

      e match {
        case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
          val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)
          val optimized: Option[Exp[_]] = collectFirst(conds)(tryGroupBy(OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]]), conds, f)(_))
          optimized.getOrElse(e)
        case _ => e
      }
    }
  }

  /*
  val groupByShare: Exp[_] => Exp[_] = {
    e => e match {
      case Filter(View(c: Exp[Traversable[_ /*t*/]]), (f: FuncExp[t, _ /*Boolean*/]) & FuncExpBody(fEqBody: Eq[t2])) =>
        val Eq(lhs, rhs) = fEqBody
        val coll = OptimizationTransforms.stripView(c.asInstanceOf[Exp[Traversable[t]]])
        if (rhs.isOrContains(f.x) && !lhs.isOrContains(f.x))
          groupByShareBody[t, t2](coll, f, fEqBody, lhs, rhs).getOrElse(e)
        else if (lhs.isOrContains(f.x) && !rhs.isOrContains(f.x))
          groupByShareBody[t, t2](coll, f, fEqBody, rhs, lhs).getOrElse(e)
        else
          e
      case _ => e
    }
  }
  */

  //Entry point
  def shareSubqueries[T](query: Exp[T]): Exp[T] = {
    query.transform(directsubqueryShare.andThen(groupByShare2))
  }
}
