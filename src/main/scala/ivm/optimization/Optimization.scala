package ivm
package optimization

import expressiontree._
import Lifting._
import collection.generic.FilterMonadic
import collection.GenTraversableOnce
object FuncExpBody {
  def unapply[S, T](f: FuncExp[S, T]): Option[Exp[T]] = Some(f.body)
}

object FuncExpIdentity {
  def unapply[S, T](f: FuncExp[S, T]): Boolean = f.body == f.x
}
object & { def unapply[A](a: A) = Some(a, a) }
class Optimization {
  private def buildJoin[T, S, TKey, TResult](fmColl: Exp[Traversable[T]],
                                                  wfColl: Exp[Traversable[S]],
                                                  lhs: Exp[TKey], rhs: Exp[TKey],
                                                  moFun: FuncExp[S, TResult], fmFun: FuncExp[T, GenTraversableOnce[TResult]],
                                                  wfFun: FuncExp[S, Boolean]): Exp[Traversable[TResult]] /*Join[T, S, TKey, TResult]*/ = {
    fmColl.join(
      wfColl,
      FuncExp.makefun[T, TKey](lhs, fmFun.x).f,
      FuncExp.makefun[S, TKey](rhs, wfFun.x).f,
      FuncExp.makepairfun[T, S, TResult](
        moFun.body,
        fmFun.x,
        moFun.x).f)
  }

  // First solution which worked in the end. Of course, it doesn't rebind t. I could return it casted, but then I
  // couldn't use this easily in a pattern guard.
  //def hasType[T: ClassManifest](t: Exp[_]): Boolean = t.manifest <:< classManifest[T]

  //Second solution: this pattern matcher must be used with &, as in
  // (fmColl: Exp[Traversable[_]]) & TypedExp(TraversableManifest)
  object TypedExp {
    //def unapply[_](t: Exp[_]): Option[ClassManifest[_]] = Some(t.manifest)
  }

  val TraversableManifest: ClassManifest[Traversable[_]] = classManifest[Traversable[_]]

  //This final solution rebinds Exp while matching on its manifest.
  /*def typedExpMatcher[T: ClassManifest] = new AnyRef {
    def unapply(t: Exp[_]): Option[Exp[T]] =
      if (t.manifest == classManifest[T])
        Some(t.asInstanceOf[Exp[T]])
      else
        None
  }*/
  //val TraversableExp = typedExpMatcher[Traversable[Any]]
  // Implementation note: if I use Traversable[_] type inference cannot deduce a specific type parameter for
  // buildJoinTyped, hence let's use Any instead of _ to make type inference deduce Any.

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
        fmFun @ FuncExpBody(MapOp(WithFilter(wfColl: Exp[Traversable[_]], wfFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
        if !wfColl.isOrContains(fmFun.x)
      =>
        //XXX: buildJoin is passed Any as type parameter - this makes manifests for Any be part of the new expression.
        // It's not so bad because we'll produce a res: Exp[Traversable[Any]] - the lost information has little relevance
        // currently: the manifest still shows that result represents a Traversable.
        if (!(lhs.isOrContains(wfFun.x)) && !(rhs.isOrContains(fmFun.x)))
          buildJoin(fmColl, wfColl, lhs, rhs, moFun, fmFun, wfFun)
        else if (!(rhs.isOrContains(wfFun.x)) && !(lhs.isOrContains(fmFun.x)))
          buildJoin(fmColl, wfColl, rhs, lhs, moFun, fmFun, wfFun)
        else
          e
      case _ => e
    }

  // KO: Is this transformation typesound? If we have a collection c of type Exp[FilterMonadic...] and
  // transform c.map(identity).groupBy(...) to c.groupBy(...) then we have a problem because groupBy
  // is not available on FilterMonadic...
  val removeIdentityMaps: Exp[_] => Exp[_] =
    e => e match {
      case MapOp(col, FuncExpIdentity()) =>
        // XXX: the cast in the line below is only needed because of a compiler bug (yet to report), which only
        // shows up when recompiling this class but not MapOp. Another (now fixed) bug with separate compilation
        // is described here: https://issues.scala-lang.org/browse/SI-4757
        col.asInstanceOf[Exp[_]]
      case MapOp(col, FuncExpBody(x)) =>
        println(x); e
      case _ => e
    }


  val mergeFilters: Exp[_] => Exp[_] =
    e => e match {
      case WithFilter(WithFilter(col2: Exp[FilterMonadic[t, _]], f2), f) =>
        // Here, the cast is needed to make the type to match the one of the implicit conversion and thus to enable the
        // latter.
        mergeFilters(
          col2.asInstanceOf[Exp[FilterMonadic[t, Traversable[t]]]].withFilter{
            (x: Exp[_]) => And(f2(x), f(x))
          })
      case _ => e
    }

  val normalizer: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
      case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
      case _ => e
    }

}

import scala.collection.mutable.Map

object Optimization {
  val opt = new Optimization()
  val subqueries : Map[Exp[_],Any] = Map.empty
  def addSubQuery[T](query: Exp[T]) {
    val optquery = optimize(query)  // TODO: Reconsider whether it is a good idea to optimize here
    subqueries += optquery -> optquery.interpret()
  }
  def removeSubQuery[T](query: Exp[T]) {
    subqueries -=  query
  }
  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(opt.cartProdToJoin)

  def optimize[T](exp: Exp[T]): Exp[T] = {
    shareSubqueries(
     mergeFilters(
      optimizeCartProdToJoin(exp)))
  }

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(opt.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = exp.transform(opt.mergeFilters)

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(opt.removeIdentityMaps)

  def shareSubqueries[T](query: Exp[T]) : Exp[T] = {
      new SubquerySharing(subqueries).shareSubqueries(query)
  }

}
