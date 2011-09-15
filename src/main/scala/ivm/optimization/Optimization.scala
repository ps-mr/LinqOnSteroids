package ivm
package optimization


import expressiontree._
import indexing.{HashIndex, Path}

object FuncExpBody {
  def unapply[S, T](f: FuncExp[S, T]): Option[Exp[T]] = Some(f.body)
}

object FuncExpIdentity {
  def unapply[S, T](f: FuncExp[S, T]): Boolean = f.body == f.x
}

class Optimization {
  //Note on the type signature: I commented out type parameters which the type checker does not check.
  private def buildJoinTyped[T, S, TKey, TResult](fmColl: QueryReifier[T], wfColl: QueryReifier[S],
                                                  lhs: Exp[TKey], rhs: Exp[TKey],
                                                  moFun: FuncExp[S, TResult], fmFun: FuncExp[T, QueryReifier[TResult]],
                                                  wfFun: FuncExp[S, Boolean]): QueryReifierBase[TResult] /*Join[T, S, TKey, TResult]*/ =
    fmColl.join(
      wfColl,
      FuncExp.makefun[T, TKey](lhs, fmFun.x).f,
      FuncExp.makefun[S, TKey](rhs, wfFun.x).f,
      FuncExp.makepairfun[T, S, TResult](
        moFun.body,
        fmFun.x,
        moFun.x).f)

  /*
   * Optimizes expressions of the form:
   *   for (k <- l; k2 <- j if l(k) is r(k2)) yield mcf(k, k2)
   * that is:
   *   l.flatMap(k => j.withFilter(l(k) is r(_)).map(mcf(k, _)))
   * into:
   *   l.join(j, l, r, (p: Exp[(Int, Int)]) => mcf(p._1, p._2))
   */
  val cartProdToJoin: Exp[_] => Exp[_] =
    e => e match {
      case FlatMap(fmColl,
        fmFun @ FuncExpBody(MapOp(WithFilter(wfColl, wfFun @ FuncExpBody(Eq(lhs, rhs))), moFun)))
        if (!wfColl.isOrContains(fmFun.x))
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
      case WithFilter(WithFilter(col2, f2), f) =>
        mergeFilters(col2.withFilter(FuncExp((x: Exp[_]) => And(f2.f(x), f.f(x))).f))
      case _ => e
    }

  val normalizer: Exp[_] => Exp[_] =
    e => e match {
      case p@Plus(x, y) => Plus(Exp.min(x, y), Exp.max(x, y))(p.isNum)
      case e@Eq(x, y) => Eq(Exp.min(x, y), Exp.max(x, y))
      case _ => e
    }

  private def hasIndex(idx: scala.collection.mutable.Map[FuncExp[Any, Any], HashIndex[Any, Any]], hx: Var, l: Exp[_]): Boolean = {
    idx.contains(Optimization.normalize(FuncExp.makefun(l, hx)).asInstanceOf[FuncExp[Any, Any]])
  }

  private def getPath[T, S](col: QueryReifier[T]): Option[(Path[(S, _), T], Traversable[S]) /*forSome {type S}*/ ] = {
    col match {
      case f: FlatMap[q, _ /*T*/ ] => {
        getPath[q, S](f.col) match {
          case Some((a, b)) => None
          case _ => None
        }
      }

      case _ => None
    }
  }

  def canUseIndex[T, U](col: QueryReifier[T], h: FuncExp[T, Boolean], l: Exp[U], r: Exp[U]) =
    !l.isOrContains(h.x) && r.freeVars == Set(h.x) &&
      hasIndex(col.indexes.asInstanceOf[scala.collection.mutable.Map[FuncExp[Any, Any], HashIndex[Any, Any]]], h.x, r)

  def buildIndexAt[T, U](col: QueryReifier[T], h: FuncExp[T, Boolean], l: Exp[U], r: Exp[U]) =
    IndexAt(col.indexes(Optimization.normalize(FuncExp.makefun(r, h.x)).asInstanceOf[FuncExp[Any, Any]]).asInstanceOf[HashIndex[Any, Any]], l)

  val indexer: Exp[_] => Exp[_] = (e) => e match {
    case WithFilter(col, h @ FuncExpBody(Eq(l, r))) =>
      if (canUseIndex(col, h, l, r))
        buildIndexAt(col, h, l, r)
      else if (canUseIndex(col, h, r, l))
        buildIndexAt(col, h, r, l)
      else
        e
    case _ => e
  }
}


object Optimization {
  val opt = new Optimization()

  def optimizeCartProdToJoin[T](exp: Exp[T]): Exp[T] = exp.transform(opt.cartProdToJoin)

  def optimize[T](exp: Exp[T]): Exp[T] = optimizeCartProdToJoin(exp)

  def optimizeIndexing[T](exp: Exp[T]): Exp[T] = exp.transform(opt.indexer)

  def normalize[T](exp: Exp[T]): Exp[T] = exp.transform(opt.normalizer)

  def mergeFilters[T](exp: Exp[T]): Exp[T] = exp.transform(opt.mergeFilters)

  def removeIdentityMaps[T](exp: Exp[T]): Exp[T] = exp.transform(opt.removeIdentityMaps)
}
