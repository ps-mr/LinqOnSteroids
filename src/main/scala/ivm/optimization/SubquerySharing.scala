package ivm.optimization

import ivm.expressiontree._
import Lifting._

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

/*        case WithFilter(c, f @ FuncExpBody(Eq(lhs, rhs))) if (rhs.isOrContains(f.x) && !lhs.isOrContains(f.x))=> {
          subqueries.get(c.groupBy(FuncExp.makefun(rhs, f.x))) match {
            case Some(t) => App(Const(t.asInstanceOf[ Any => _]), lhs)
            case None => e
          }
        }  */
        case _ => e
    }
  }

  def shareSubqueries[T](query: Exp[T]) : Exp[T] = {
      query.transform(directsubqueryShare.andThen(groupByShare))
  }
}
