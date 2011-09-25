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

        case WithFilter(c: Exp[FilterMonadic[_ /*t*/, _]], (f: FuncExp[t, _/*Boolean*/]) & FuncExpBody(Eq(lhs, rhs))) if (rhs.isOrContains(f.x) && !lhs.isOrContains(f.x))=> {
          f match {
            case f2 : Eq[t2] =>
              implicit val cmT: ClassManifest[t] = f.cmS
              val cmT2 = f2.x.manifest.asInstanceOf[ClassManifest[t2]]
              subqueries.get(
                                     c.asInstanceOf[Exp[FilterMonadic[t, Traversable[t]]]]
                                      .map[t,Traversable[t]](identity)(implicitly, cmT, implicitly, implicitly)
                                      .groupBy(FuncExp.makefun[t,t2](f2.y, f.x)(implicitly, cmT2).f)(implicitly, cmT2)) match {
                                         case Some(t) => App(Const(t.asInstanceOf[Any => Traversable[t] /*?*/]), lhs)(implicitly)
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
