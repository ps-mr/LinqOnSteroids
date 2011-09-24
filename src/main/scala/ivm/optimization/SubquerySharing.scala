package ivm.optimization

import ivm.expressiontree._
import Lifting._
import collection.generic.FilterMonadic

class SubquerySharing(val subqueries: Map[Exp[_],_]) {
   val directsubqueryShare: Exp[_] => Exp[_] = {
      (e) => subqueries.get(e) match {
        case Some(t) => Const(t)
        case None => e
      }
    }
   val groupByShare: Exp[_] => Exp[_] = {
    (e) => e match {
        // KO: Paolo, maybe you can fix the two compiler errors in this code? They are both related to your encoding.

        case WithFilter(c : Exp[FilterMonadic[t, _]], f @ FuncExpBody(Eq(lhs, rhs))) if (rhs.isOrContains(f.x) && !lhs.isOrContains(f.x))=> {
          f match {
            case f2 : Eq[t2] => subqueries
                                   .get(
                                     c.asInstanceOf[Exp[FilterMonadic[t, Traversable[t]]]]
                                      .map[t,Traversable[t]]( (x : Exp[t]) => x)(null,null,null,null)
                                      .groupBy(FuncExp.makefun[t,t2](f2.y, f.x)(null,null).f)(null,null)) match {
                                         case Some(t) => App(Const(t.asInstanceOf[ Any => _]), lhs)(null)
                                         case None => e
                                      }
          }
        }
        case _ => e
    }
  }

  def shareSubqueries[T](query: Exp[T]) : Exp[T] = {
      query.transform(directsubqueryShare.andThen(groupByShare))
  }
}
