package ivm
package optimization


import expressiontree._
import expressiontree.Lifting._
import indexing.HashIndex

class Optimization {
  val cartProdToJoin : Exp[_] => Exp[_] =
     (e) => e match {
       case FlatMap(fmcol,fmf) => fmf.f(fmf.x) match {
         case Map(mccol,mcf) => mccol match {
           case WithFilter(col3,h) => {
             if (col3.isOrContains(fmf.x)) e else 
             h.f(h.x) match {
               case Eq(l,r) => if (!(l.isOrContains(h.x)) && !(r.isOrContains(fmf.x)))
                                   Join(fmcol, 
                                        col3, 
                                        FuncExp.makefun[Any,Any](l, fmf.x),  // no idea why the [Any,Any] stuff passes the typechecker
                                        FuncExp.makefun[Any,Any](r, h.x),
                                        FuncExp.makepairfun[Any,Any,Any](
                                                mcf.f(mcf.x), 
                                                fmf.x, 
                                                mcf.x))
                               else if (!(r.isOrContains(h.x)) && !(l.isOrContains(fmf.x)))
                                   Join(fmcol, 
                                        col3, 
                                        FuncExp.makefun[Any,Any](r, fmf.x), 
                                        FuncExp.makefun[Any,Any](l, h.x),
                                        FuncExp.makepairfun[Any,Any,Any](
                                                mcf.f(mcf.x), 
                                                fmf.x, 
                                                mcf.x))
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

  val mergeFilters : Exp[_] => Exp[_] =
    (e) => e match {
      case WithFilter(col,f) =>
        col match {
          case WithFilter(col2,f2) =>
            mergeFilters(WithFilter(col2, FuncExp( (x:Exp[_]) => And(f2(x),f(x)))))
          case _ => e
        }
      case _ => e
    }

  val normalizer : Exp[_] => Exp[_] = 
     (e) => e match {
       case p@Plus(x,y) => Plus(Exp.min(x,y), Exp.max(x,y))(p.sum)
       case e@Eq(x,y) => Eq(Exp.min(x,y), Exp.max(x,y))
       case _ => e
     }
     
  private def hasIndex(idx: scala.collection.mutable.Map[FuncExp[Any,Any],HashIndex[Any,Any]], hx: Var[_], l: Exp[_]) : Boolean = {
    idx.contains(Optimization.normalize(FuncExp.makefun(l, hx)).asInstanceOf[FuncExp[Any,Any]])
  }
  val indexer : Exp[_] => Exp[_] =  (e) => e match {
       case WithFilter(col,h)  => h.f(h.x) match {
         case Eq(l,r) => if ((!(l.isOrContains(h.x))) && (r.freeVars == Seq(h.x)) 
                              && hasIndex(col.indexes.asInstanceOf[scala.collection.mutable.Map[FuncExp[Any,Any],HashIndex[Any,Any]]], h.x, r)) 
                         IndexAt(col.indexes(Optimization.normalize(FuncExp.makefun(r, h.x)).asInstanceOf[FuncExp[Any,Any]]).asInstanceOf[HashIndex[Any,Any]], l)
                         else 
                         if ((!(r.isOrContains(h.x))) 
                             && (l.freeVars.equals(Set(h.x)))
                              && hasIndex(col.indexes.asInstanceOf[scala.collection.mutable.Map[FuncExp[Any,Any],HashIndex[Any,Any]]], h.x, l)) 
                         IndexAt(col.indexes(Optimization.normalize(FuncExp.makefun(l, h.x)).asInstanceOf[FuncExp[Any,Any]]).asInstanceOf[HashIndex[Any,Any]], r)
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
  def optimizeIndexing[T](exp: Exp[T]) : Exp[T] = exp.transform(opt.indexer)
  def normalize[T](exp: Exp[T]) : Exp[T] = exp.transform(opt.normalizer)
  def mergeFilters[T](exp: Exp[T]) : Exp[T] = exp.transform(opt.mergeFilters)
}
