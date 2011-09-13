package ivm
package optimization


import expressiontree._
import indexing.{HashIndex, Path}

class Optimization {
  //Note on the type signature: I commented out type parameters which the type checker does not check.
  private def buildJoinTyped[T, S, TKey, TResult](fmcol: QueryReifier[T], col3: QueryReifier[S],
                                                  l: Exp[TKey], r: Exp[TKey],
                                                  mcf: FuncExp[_ /*U*/, TResult], fmf: FuncExp[_ /*T*/, _ /*QueryReifier[U]*/],
                                                  h: FuncExp[_ /*S*/, _/*Boolean*/]): Join[T, S, TKey, TResult] =
    Join(fmcol,
      col3,
      FuncExp.makefun[T, TKey](l, fmf.x),
      FuncExp.makefun[S, TKey](r, h.x),
      FuncExp.makepairfun[T, S, TResult](
        mcf.body,
        fmf.x,
        mcf.x))

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
      case FlatMap(fmcol: QueryReifier[t], fmf: FuncExp[_ /*t*/, QueryReifier[u]]) => fmf.body match {
        case MapOp(mccol: QueryReifier[u2 /* u */], mcf: FuncExp[_ /* u2 */, tResult]) => mccol match {
          case WithFilter(col3: QueryReifier[s], h: FuncExp[_ /*s*/, _ /*Boolean*/]) => {
            if (col3.isOrContains(fmf.x)) e
            else
              h.body match {
                case eq: Eq[tKey] =>
                  //val Eq(l: Exp[tKey], r /*: Exp[tKey] */) = eq
                  val Eq(l, r) = eq
                  if (!(l.isOrContains(h.x)) && !(r.isOrContains(fmf.x)))
                    buildJoinTyped[t, s, tKey, tResult](fmcol, col3, l.asInstanceOf[Exp[tKey]], r.asInstanceOf[Exp[tKey]], mcf, fmf, h)
                  else if (!(r.isOrContains(h.x)) && !(l.isOrContains(fmf.x)))
                    buildJoinTyped[t, s, tKey, tResult](fmcol, col3, r.asInstanceOf[Exp[tKey]], l.asInstanceOf[Exp[tKey]], mcf, fmf, h)
                  else e
                case _ => e
              }
          }
          case _ => e
        }
        case _ => e
      }
      case _ => e
    }

  val removeIdentityMaps: Exp[_] => Exp[_] =
    (e) => e match {
      case MapOp(col, f) =>
        f.body match {
          case f.x => col
          case x => println(x); e
        }
      case _ => e
    }

  val mergeFilters: Exp[_] => Exp[_] =
    (e) => e match {
      case WithFilter(col, f) =>
        col match {
          case WithFilter(col2, f2) =>
            mergeFilters(new WithFilterMaintainerExp(col2, FuncExp((x: Exp[_]) => And(f2.f(x), f.f(x)))))
          case _ => e
        }
      case _ => e
    }

  val normalizer: Exp[_] => Exp[_] =
    (e) => e match {
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

  val indexer: Exp[_] => Exp[_] = (e) => e match {
    case WithFilter(col, h) => h.body match {
      case Eq(l, r) => if ((!(l.isOrContains(h.x))) && (r.freeVars == Seq(h.x))
        && hasIndex(col.indexes.asInstanceOf[scala.collection.mutable.Map[FuncExp[Any, Any], HashIndex[Any, Any]]], h.x, r))
        IndexAt(col.indexes(Optimization.normalize(FuncExp.makefun(r, h.x)).asInstanceOf[FuncExp[Any, Any]]).asInstanceOf[HashIndex[Any, Any]], l)
      else
      if ((!(r.isOrContains(h.x)))
        && (l.freeVars.equals(Set(h.x)))
        && hasIndex(col.indexes.asInstanceOf[scala.collection.mutable.Map[FuncExp[Any, Any], HashIndex[Any, Any]]], h.x, l))
        IndexAt(col.indexes(Optimization.normalize(FuncExp.makefun(l, h.x)).asInstanceOf[FuncExp[Any, Any]]).asInstanceOf[HashIndex[Any, Any]], r)
      else e

      case _ => e
    }
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
