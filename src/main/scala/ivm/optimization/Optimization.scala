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
  private def buildJoinTyped[T, S, TKey: ClassManifest, TResult: ClassManifest](fmColl: Exp[Traversable[T]],
                                                  wfColl: Exp[Traversable[S]],
                                                  lhs: Exp[TKey], rhs: Exp[TKey],
                                                  moFun: FuncExp[S, TResult], fmFun: FuncExp[T, GenTraversableOnce[TResult]],
                                                  wfFun: FuncExp[S, Boolean]): Exp[Traversable[TResult]] /*Join[T, S, TKey, TResult]*/ = {
    implicit val cmT = fmFun.cmS
    implicit val cmS = moFun.cmS
    fmColl.join(
      wfColl,
      FuncExp.makefun[T, TKey](lhs, fmFun.x).f,
      FuncExp.makefun[S, TKey](rhs, wfFun.x).f,
      FuncExp.makepairfun[T, S, TResult](
        moFun.body,
        fmFun.x,
        moFun.x).f)
  }

  /*
  def TypedExp[T: ClassManifest] = new AnyRef {
    def unapply(t: Exp[_]): Option[Exp[T]] = if (t.manifest == classManifest[T]) Some(t.asInstanceOf[Exp[T]]) else None
  }
  object TypedExp {
    //def unapply[T](t: Exp[_]): Option[Exp[T]] = if (t.manifest == classManifest[T]) Some(t.asInstanceOf[Exp[T]]) else None
    //def unapply[T](t: Exp[_]): Option[Exp[T]] = Some(t.asInstanceOf[Exp[T]])
    def unapply[T](t: Exp[_]): Boolean = true
  }

  object TypedExp {
    def unapply[T: ClassManifest](t: Exp[_]): Option[Exp[T]] = if (t.manifest == classManifest[T]) Some(t.asInstanceOf[Exp[T]]) else None
    //def unapply[T](t: Exp[_]): Option[ClassManifest[T]] = Some(t.manifest)
  }
  */
  /*
  //Last, best version:
  object TypedExp {
    def unapply[T](t: Exp[T]): Option[ClassManifest[T]] = Some(t.manifest.asInstanceOf[ClassManifest[T]])
  }
  //But it's strictly typed, so it does not work with &.
  */
  object TypedExp {
    def unapply[_](t: Exp[_]): Option[ClassManifest[_]] = Some(t.manifest)
  }

  val TraversableManifest: ClassManifest[Traversable[_]] = classManifest[Traversable[_]]
  /*def matcher[T](manifest: ClassManifest[T]) = new AnyRef {
    def unapply(t: Exp[_]): Boolean = if (t.manifest == classManifest[T]) Some(t.asInstanceOf[Exp[T]]) else None
  }
  val TraversableExp = matcher(TraversableManifest)*/
  def typedExpMatcherBind[T: ClassManifest] = new AnyRef {
    def unapply(t: Exp[_]): Option[Exp[T]] = if (t.manifest == classManifest[T]) Some(t.asInstanceOf[Exp[T]]) else None
  }
  val TraversableExpBind = typedExpMatcherBind[Traversable[Any]] //If I use Traversable[_] type inference cannot deduce
  //a specific type parameter for buildJoinTyped, hence let's use Any instead of _ to make type inference deduce Any.
  def typedExpMatcher[T: ClassManifest] = new AnyRef {
    def unapply(t: Exp[_]): Boolean =
      if (t.manifest == classManifest[T])
        true
      else
        false
  }
  val TraversableExp = typedExpMatcher[Traversable[_]]
  // define extractors for TraversableExp and so on. That's less ugly than having this classManifest thing - since one
  // can't have expressions in a pattern match, apparently.

  // Only solution which worked in the end. Of course, it doesn't rebind t. I could return it casted, but then I
  // couldn't use this easily in a pattern guard.
  def hasType[T: ClassManifest](t: Exp[_]): Boolean = t.manifest <:< classManifest[T]

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
      /*case FlatMap(fmColl @ TypedExp(TraversableManifest), //: Exp[Traversable[_]],
        fmFun @ FuncExpBody(MapOp(WithFilter(wfColl: Exp[Traversable[_]], wfFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
        if !wfColl.isOrContains(fmFun.x) && hasType[Traversable[_]](fmColl) && hasType[Traversable[_]](wfColl)*/
      case FlatMap(TraversableExpBind(fmColl),
        fmFun @ FuncExpBody(MapOp(WithFilter(TraversableExpBind(wfColl), wfFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
        if !wfColl.isOrContains(fmFun.x)// && hasType[Traversable[_]](fmColl) && hasType[Traversable[_]](wfColl)
      =>
        if (!(lhs.isOrContains(wfFun.x)) && !(rhs.isOrContains(fmFun.x)))
          buildJoinTyped(fmColl, wfColl, lhs, rhs, moFun, fmFun, wfFun)
        else if (!(rhs.isOrContains(wfFun.x)) && !(lhs.isOrContains(fmFun.x)))
          buildJoinTyped(fmColl, wfColl, rhs, lhs, moFun, fmFun, wfFun)
        else
          e
      case _ => e
    }
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
        mergeFilters(
          col2.asInstanceOf[Exp[FilterMonadic[t, Traversable[t]]]].withFilter{
            (x: Exp[_]) => And(f2(x), f(x))
          }(f2.cmS.asInstanceOf[ClassManifest[t]]))
      case _ => e
    }

  val normalizer: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum, p.cm)
      case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
      case _ => e
    }

}


object Optimization {
  val opt = new Optimization()

  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(opt.cartProdToJoin)

  def optimize[T](exp: Exp[T]): Exp[T] = optimizeCartProdToJoin(exp)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(opt.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = exp.transform(opt.mergeFilters)

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(opt.removeIdentityMaps)
}
