package ivm.optimization

import ivm.expressiontree._
import Lifting._
import Util._
import collection.generic.FilterMonadic

class SubquerySharing(val subqueries: Map[Exp[_],_]) {
   val directsubqueryShare: Exp[_] => Exp[_] = {
      (e) => subqueries.get(e) match {
        case Some(t) => Const(t)
        case None => e
      }
    }

   private def groupByShareBody[T: ClassManifest, T2: ClassManifest](c: Exp[FilterMonadic[T, Traversable[T]]],
                                                        f: FuncExp[T, Boolean], fEqBody: Eq[T2],
                                                        completeExp: Exp[_]) = {
     /*
     //How the fuck does this typecheck?
     val groupedBy2: Exp[T => Traversable[T]] = c.map(identity).groupBy(FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     //Reason: a FuncExp[T, T2] is also FuncExp[T, Any]; groupBy(f: FuncExp[T, Any]) gives Map[Any, Repr]
     // (where Repr = Traversable[T] here) which can be _upcast_ to Any => Repr and then further upcast to T => Repr.
     // It's an interesting anomaly of type inference with variance, but it does not produce unsoundness.
     // Indeed, since Map[A, +B] is invariant in A, we can't upcast Map[Any, Repr] to Map[T, Repr] - see groupedBy5.
     //val groupedBy3: Exp[T => Traversable[T]] = c.map(identity).groupBy[T2](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     val groupedBy4: Exp[T => Traversable[T]] = c.map(identity).groupBy[Any](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     //
     val groupedBy5: Exp[Map[Any, Traversable[T]]] = c.map(identity).groupBy[Any](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     */
     //This code:
     //val groupedBy = c.map(identity).groupBy(FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     //expands to this:
     val groupedBy = c.map(identity).groupBy[T2](FuncExp.makefun[T, T2](fEqBody.y, f.x).f)
     
     assertType[Exp[T2 => Traversable[T]]](groupedBy) //Just for documentation.

     subqueries.get(groupedBy) match {
       case Some(t) => App(Const(t.asInstanceOf[T2 => Traversable[T]]), fEqBody.x)
       case None => completeExp
     }
   }


   val groupByShare: Exp[_] => Exp[_] = {
    (e) => e match {
        // KO: Paolo, maybe you can fix the two compiler errors in this code? They are both related to your encoding.

        case WithFilter(c: Exp[FilterMonadic[_ /*t*/, _]], (f: FuncExp[t, _/*Boolean*/]) & FuncExpBody(Eq(lhs, rhs)))
          if (rhs.isOrContains(f.x) && !lhs.isOrContains(f.x)) => {
          f.body match {
            case fEqBody: Eq[t2] =>
              val cmT: ClassManifest[t] = f.cmS
              val cmT2 = fEqBody.x.manifest.asInstanceOf[ClassManifest[t2]] //we have ClassManifest[_ <: t2], hence we need the cast :-(.
              groupByShareBody[t, t2](c.asInstanceOf[Exp[FilterMonadic[t, Traversable[t]]]], f, fEqBody, e)(cmT, cmT2)
          }
        }
        case _ => e
    }
  }

  def shareSubqueries[T](query: Exp[T]) : Exp[T] = {
      query.transform(directsubqueryShare.andThen(groupByShare))
  }
}
