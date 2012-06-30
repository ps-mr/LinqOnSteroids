package ivm
package optimization

import expressiontree._
import Lifting._
import annotation.tailrec
import OptimizationUtil._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait Unnesting {
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
}
