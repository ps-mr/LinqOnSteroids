package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._
import BetterPartialOrdering._
import collection.generic.CanBuildFrom

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait Unnesting {
  //This simplification cleans up after existsUnnester and existsRenester.
  val resimplFilterIdentity: Exp[_] => Exp[_] = {
    case IsEmpty(FlatMap(coll, f @ FuncExpBody(Filter(coll2, FuncExpIdentity())))) =>
      (coll flatMap Fun.makefun(coll2, f.x)).isEmpty
    case e => e
  }

  private def cbfStaticResult[From, Elem, To](cbf: CanBuildFrom[From, Elem, To]): To =
    cbf.apply().result()

  private def isSet[T](t: T) = t.isInstanceOf[collection.Set[_]]
  private def isMap[T](t: T) = t.isInstanceOf[collection.Map[_, _]]
  private def isSeq[T](t: T) = t.isInstanceOf[collection.Seq[_]] //Untested

  def isCbfCommutative[From, Elem, To](cbf: CanBuildFrom[From, Elem, To]) = {
    isSet(cbfStaticResult(cbf))
  }

  def isCbfIdempotent[From, Elem, To](cbf: CanBuildFrom[From, Elem, To]) = {
    val cbfRes = cbfStaticResult(cbf)
    isSet(cbfRes) || isMap(cbfRes)
  }

  /*
   * This ordering expresses the ordering between monoids described by Fegaras and Maier (2001) in
   * "Optimizing Object Queries Using an Effective Calculus", Sec. 2.3.
   * It can be used to reject non-well-defined expressions such as someSet.toList, where the order of the result is
   * undefined, and to avoid query unnesting when it would change the query semantics - for instance, unnesting a
   * subquery producing a set into a query producing a list would remove a duplicate elimination step.
   */
  private implicit def cbfOrdering[From, Elem, To]: PartialOrderingExt[CanBuildFrom[From, Elem, To]] = { /*Ordering[CanBuildFrom[_, _, _]]*/
    def flagsFrom(cbf: CanBuildFrom[From, Elem, To]): (Boolean, Boolean) =
      (isCbfCommutative(cbf), isCbfIdempotent(cbf))
    PartialOrderingExt[(Boolean, Boolean)].on(flagsFrom)
  }
  //Finally, correct unnesting.
  val existsUnnester: Exp[_] => Exp[_] = {
    //Rule N9 page 474 in Optimizing Object Queries Using an Effective Calculus, Fegaras and Maier, 2000:
    //c0 withFilter (x0 => c exists (x => p(x))) flatMap (x1 => B [not free in x]) |=> c0 flatMap (x0 => c withFilter (x => p(x)) flatMap restQuery)
    //where restQuery = x => [x1 |-> x0]B
    //However, that's only valid if the return value of the expression is an idempotent monoid, as stated there. We can workaround that by converting
    //the result of flatMap to a set.
    case fm @ FlatMap(Filter(c0, f@FuncExpBody(Not(IsEmpty(Filter(c, p))))), fmFun) =>
      //Since x0 = f.x, and fmFun = x1 => B, then [x1 |-> x0]B is (x1 => B) x0, that is fmFun(f.x), and restQuery = x => [x1 |-> x0]B =
      //val restQuery = Fun.makefun(fmFun(f.x), p.x) //Hmm, this is a bit of a hack. The scope of p.x does not extend over f.x.
      //Hence, that code might as well use a fresh variable. In fact, that's a function which ignores its argument -
      //const fmFun(f.x). Hence:
      val restQuery = Fun[Any, Traversable[Any]](x => fmFun(f.x))

      if (isCbfIdempotent(fm.c)) {
        //Use the standard rule to unnest exists into an idempotent comprehension
        stripView(c0) flatMap Fun.makefun(stripView(c) filter p flatMap restQuery, f.x)
      } else {
        //Use an original rule to unnest exists into a non-idempotent comprehension while still doing the needed duplicate
        //elimination
        //breakOut is used to fuse manually the mapping step with toSet. XXX: This fusion should be automated!!!
        //XXX: Also, we should make somehow sure that breakOut steps are preserved - they currently aren't!
        //XXX: Hence, better have an explicit toSet step for now, and rewrite such steps using breakOut in a final processing step.

        //toSet remove any duplicates to preserve semantics; in the expression c exists p, p might be true for more elements of c. When unnesting,
        //we produce c withFilter p, and for each element in the result we apply x1 => B. Instead, with toSet we unify the results after applying
        //filtering. That's unsatisfactory though; we could instead of using toSet on the result, use breakout as CanBuildFrom instance on the last flatMap.
        //A problem, in both cases, is that the result of this transformation looks slower than the original query.
        //Is duplicate elimination after applying restQuery valid? I guess not: the flatMapped function might just produce an element multiple times.
        //So we produce instead a Set of booleans to do duplicate elimination and then filter with identity!
        stripView(c0) flatMap Fun.makefun((((stripView(c) map p)(collection.breakOut): Exp[Set[Boolean]]) filter identity flatMap restQuery)(collection.breakOut): Exp[Traversable[Any]], f.x)
      }
    case e => e
  }

  private def rebuildExists[T, U, V](coll: Exp[Traversable[T]],
                                  coll2: Exp[Traversable[U]],
                                  p: Fun[U, Boolean],
                                  f: Fun[T, Traversable[V]],
                                  f2: Fun[U, Traversable[V]]): Exp[Traversable[V]] =
    coll filter Fun.makefun(coll2 exists p, f.x) flatMap Fun.makefun(f2.body, f.x)

  //This is useful, because exists has a more efficient execution strategy than the nested iteration: exists can stop
  //as soon as the existence witness is found.
  val existsRenester: Exp[_] => Exp[_] = {
    case e @ FlatMap(coll, f@FuncExpBody(FlatMap(Filter(coll2, p), f2))) if !f2.body.isOrContains(f2.x) =>
      rebuildExists(coll, coll2, p, f, f2)
    case e => e
  }

  //A safety condition is that we must reject a nested collection with stronger algebraic laws;
  //in the monoid comprehension calculus this is required for an expression to be _syntactically_ valid, but here it is
  //not.
  //For instance, one should not unnest a subquery creating a set (hence performing duplicate elimination) nested into
  //a query creating a list.
  val generalUnnesting: Exp[_] => Exp[_] = {
    /*
     * v = E' map (x' => e')
     * v flatMap (y => e) |-> E ' flatMap (x' => letExp(e')(y => e))
     * v filter (y => p) |-> ...
     */
    /*
    //The safety condition can (and must) be checked only for FlatMap - hence it looks harder to share code between the
    //two branches below, like this:
    case BaseBindingWithT(FlatMap(collEp, fxp@FuncExpBody(ep)), fy@FuncExpBody(e), kind) =>
      collEp flatMap
        Fun.makefun(generalUnnesting(
          // XXX: Maybe I should use Either in the result of BaseBindingWithT to avoid the need for this inelegant switch
          // and the casts
          kind match {
            case FlatMapId => ep flatMap fy.asInstanceOf[Fun[Any, Traversable[Any]]]
            case FilterId => ep filter fy.asInstanceOf[Fun[Any, Boolean]]
            case _ => throw new RuntimeException
          }).asInstanceOf[Exp[Traversable[Any]]], fxp.x)
          */
    case outer @ FlatMap(inner @ FlatMap(collEp, fxp@FuncExpBody(ep)), fy@FuncExpBody(e))
      if { import PartialOrderingExt.Implicits._; inner.c <= outer.c } =>
      //Why and when do we call generalUnnesting again?
      //Subexpressions are already optimized, but subexpressions we build are not. In particular, if ep is a FlatMap node, when we invoke flatMap/filter on ep the result might require further unnesting.
      collEp flatMap Fun.makefun(generalUnnesting(ep flatMap fy).asInstanceOf[Exp[Traversable[Any]]], fxp.x)
    case Filter(FlatMap(collEp, fxp@FuncExpBody(ep)), fy@FuncExpBody(e)) =>
      collEp flatMap Fun.makefun(generalUnnesting(ep filter fy).asInstanceOf[Exp[Traversable[Any]]], fxp.x)
    case e => e
  }
}
