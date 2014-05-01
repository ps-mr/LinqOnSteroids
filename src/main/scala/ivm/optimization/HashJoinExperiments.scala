package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._

trait HashJoinExperiments {
  val filterToGroupBy: Exp[_] => Exp[_] = {
    /*
(R1):
coll filter (el1 => pred(proj(el1))) ==>
  (coll groupBy proj filter (pred . fst)).values.flatten

Makes no sense as it is, because how do you distinguish pred and proj? Hm, should recognize projections...
     */
    case e @ Sym(Filter(coll, pred))
      if pred.body isOrContains pred.x =>
        coll indexBy Fun.makefun(pred.body, pred.x).f apply true
    //So let's try with a more specific rule
    case e @ Sym(Filter(coll, FunSym(pred @ FuncExpBody(Sym(Eq(lhs, rhs))))))
      //XXX Gross simplification, need to consider symmetric case or avoid that with a suitable normal form.
      if (lhs isOrContains pred.x) && !(rhs isOrContains pred.x) =>
        coll indexBy Fun.makefun(lhs, pred.x).f apply rhs

    /*
(R2):
anyMap filter ((k, v) => k == k*) ==>
  anyMap get (k*) fold (Map.empty, k* -> _)
     */
    case e @ Sym(Filter(someMap, FunSym(pred @ FuncExpBody(Sym(Eq(Sym(Product2Proj1(lhs)), rhs))))))
      if lhs == pred.x =>
      //XXX test mapness
      e
    case e => e
  }
}

/*
* # Synthesizing hash-joins for O(1) IVM
* From Evernote note.

//Example 1:
(R1):
coll filter (el1 => pred(proj(el1))) ==>
  (coll indexBy proj filter (pred . fst)).values.flatten

CHECK THIS RULE WORKS ON EXAMPLES!!
*
(R2):
anyMap filter ((k, v) => k == k*) ==>
  anyMap apply (k*) map (k* -> _)
or
  (k* -> anyMap apply (k*)) transpose?
 //That transposition must go from "k* -> seq" to "seq map (k* ->)", so Map[K, Seq[V]] to Map[K, V].
(R3)
coll indexBy proj filter ((k, v) => k == k*) ==>
  coll indexBy proj apply (k*) map (k* -> _) //No special advantage here, it seems, from merging the rules. But this is easier to detect.

//Example 2 (note I'm not returning elements of the first collection, this looks bad but fixable).

coll map (el1 => coll2 filter (el2 => pred(proj(el1), el2))) ==>
  coll indexBy proj map ((k1, els1) => coll2 filter (el2 => pred(k1, el2)))

coll indexBy proj1 map ((k1, els1) => coll2 filter (el2 => k1 == proj2(el2)))) ==> [ Applying example 1 ]
e ==> e'
---------
C[e] ==> C[e']
C = coll indexBy proj1 map ((k1, els1) => [ ] )
e = coll2 filter (el2 => k1 == proj2(el2)))
e ' = (coll2 indexBy proj2 filter ((k2, v2) => k1 == k2)).values.flatten (Gotten by (R1))
e'' = (k1 -> (coll2 indexBy proj2 apply k1)).values.flatten (Gotten by (R3)).
e''' = (coll2 indexBy proj2 apply k1).flatten (Case analysis/case floating)
==>
coll indexBy proj1 map ((k1, els1) => (coll2 indexBy proj2 apply k1).flatten) //HASH JOIN (assuming coll is smaller).

// This resembles automatic indexing. Quite possibly, this is exactly automatic indexing, plus incremental view maintenance.
// Doing this is possible in some hacky way, as seen before.
// Making this perfectly elegant requires insight — but making this at all, and going to some cool workshop/conference with it, would be a cool first step.
However, right now I did not remove yet nested functions!
*
* Right now, there's a bigger problem. Before having these rewrite rules, I should ensure that the result of the rules is efficiently incrementalized — which seems obvious but then isn't.
*
*/
