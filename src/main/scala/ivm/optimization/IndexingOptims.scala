package ivm
package optimization

import expressiontree._
import OptimizationUtil._

trait IndexingOptims {
  trait Qualifier
  case class Predicate(e: Exp[Boolean]) extends Qualifier
  case class Generator[T](v: TypedVar[T], coll: Exp[Traversable[T]]) extends Qualifier
  /*
  class CommPair[T](a: T, b: T) extends Tuple2[T, T](a, b)
  object CommPair {
    def unapply[T](v: CommPair[T]) = ...
  }
  */

  class Comprehension[T](head: Exp[T], qualifiers: Seq[Qualifier]) {
    def transformed = {
      //This tries the sequence at different points. Must readd the head!
      qualifiers.tails collect {
        case Seq(Generator(v, coll), /*..., */ Predicate(Sym(Eq(a, b))))
        =>
          val MatchV = matcherAndContexts(v)
          val eqOperands = Seq(a, b)
          (for {
            side <- eqOperands
            MatchV(vAgain, proj) = side
            if guard(vAgain == v) //DEBUG
            otherSide = (eqOperands.toSet - side).head
          } yield (proj, otherSide)) match {
            case Seq((proj, otherSide)) => // Check there's a single match.
              import Lifting._
              coll indexBy proj apply otherSide
          }
        /*
        case Seq(Generator(v, coll), /*..., */ Predicate(pred))
          if pred isOrContains v
        =>
          //val MatchV = uniqueFinderAndContexts(comparer(v))
          val getEqSides = pfExpExtractor { case Sym(Eq(a, b)) => Set(a, b) }
          //val G = uniqueFinderAndContexts(getEqSides)
          pred match {
            case G(matches) =>
              for { (vOccurs, ctx) <- matches } {
                import Lifting._
                coll indexBy ctx
              }
            case _ =>
          }
          /*flatMap { eqOperands =>
            //Look for (proj v == [ ]) or ([ ] == proj v), returns the content of the hole if found.
            val results = for {
              side <- eqOperands
              MatchV(vAgain, proj) = side
              if guard(vAgain == v) //DEBUG
              otherSide = (eqOperands - side).head
            } yield (otherSide, proj)
            if (results.size == 1)
              Some(results.head)
            else
              None /*If we have two results, v appears in both, so we cannot lift the second one.
              XXX too clever. */
            /*if (eqOperands contains v)
              Some((eqOperands - v).head)
            else
              None*/
          }
          */
          /*
          //Look for (v == [ ]) or ([ ] == v), returns the content of the hole if found.
          val getEqSides = pfExpExtractor { case Sym(Eq(a, b)) => Set (a, b) } flatMap { eqOperands =>
            val results = for {
              side <- eqOperands
              MatchV(vAgain, proj) = side
              if guard(vAgain == v) //DEBUG
              otherSide = (eqOperands - side).head
            } yield (otherSide, proj)
            if (results.size == 1)
              Some(results.head)
            else
              None /*If we have two results, v appears in both, so we cannot lift the second one.
              XXX too clever. */
            /*if (eqOperands contains v)
              Some((eqOperands - v).head)
            else
              None*/
          }
          //(pfExtractor[Exp[Any], Exp[Any]] { case Sym(Eq(`v`, u)) => u; case Sym(Eq(u, `v`)) => u })
          val G = uniqueFinderAndContexts(getEqSides)
          e match {
            case G(matches) =>
              for { (vOccurs, ctx) <- matches } {
                import Lifting._
                coll indexBy ctx
              }
            case _ =>
          }
          */
          */
      }
    }
  }
}
