package ivm
package optimization


import expressiontree._
import indexing.{HashIndex, Path}

class Optimization {
  private def buildJoin(fmcol: QueryReifier[Any], col3: QueryReifier[Any], l: Exp[Any], r: Exp[Any],
                        mcf: FuncExp[Any, Any], fmf: FuncExp[Any, Any],
                        h: FuncExp[Any, Boolean]): Join[Any, Any, Any, Any] =
    Join(fmcol,
      col3,
      FuncExp.makefun[Any, Any](l, fmf.x),
      FuncExp.makefun[Any, Any](r, h.x),
      FuncExp.makepairfun[Any, Any, Any](
        mcf.body,
        fmf.x,
        mcf.x))

  val cartProdToJoin: Exp[_] => Exp[_] =
    e => e match {
      case FlatMap(fmcol, fmf) => fmf.body match {
        case MapOp(mccol, mcf) => mccol match {
          case WithFilter(col3, h) => {
            if (col3.isOrContains(fmf.x)) e
            else
              h.body match {
                case Eq(l, r) =>
                  if (!(l.isOrContains(h.x)) && !(r.isOrContains(fmf.x)))
                    buildJoin(fmcol, col3, l, r, mcf, fmf, h)
                  else if (!(r.isOrContains(h.x)) && !(l.isOrContains(fmf.x)))
                    buildJoin(fmcol, col3, r, l, mcf, fmf, h)
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
