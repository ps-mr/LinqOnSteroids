package ivm
package optimization

import expressiontree._
import OptimizationUtil._

trait IndexingOptims {
  trait Qualifier
  def pfQualExtractor[To](f: PartialFunction[Qualifier, To]) = pfExtractor(f)

  case class Predicate(e: Exp[Boolean]) extends Qualifier
  case class Generator[T](v: TypedVar[T], coll: Exp[Traversable[T]]) extends Qualifier
  /*
  class CommPair[T](a: T, b: T) extends Tuple2[T, T](a, b)
  object CommPair {
    def unapply[T](v: CommPair[T]) = ...
  }
  */

  //Implement non-deterministic matching on list elements.
  def SplitList[T, U](Ext: Extractor[T, U]): Extractor[List[T], List[(List[T], U, List[T])]] =
    extractor { list =>
      //XXX Inefficient
      val ress = (list.inits.toList zip list.tails.toList) collect {
        case (init, Ext(res) :: tail) => (init, res, tail)
      }
      if (ress.nonEmpty) Some(ress) else None
    }
  //XXX Not very useful in general, we'd need the List monad instead. This allows prototyping the client code one would write.
  def FirstSplitList[T, U](Ext: Extractor[T, U]) = SplitList(Ext) map (_.head)
  class Comprehension[T](head: Exp[T], qualifiers: Seq[Qualifier]) {
    def transformed = {
      val GenExtractor = extractor(Generator.unapply[Any] _)
      val EqPred = pfQualExtractor { case Predicate(Sym(Eq(a, b))) => Seq(a, b) }
      if (false) {
        //XXX to illustrate nested pattern matching. We should better use the List monad for matching!
        val Find1stGen = FirstSplitList(GenExtractor)
        val Find1stPred = FirstSplitList(EqPred)
        qualifiers match {
          //case Seq(/* before..., */ Generator(v, coll), /* before2 ..., */ Predicate(Sym(Eq(a, b))) /* , rest...*/)
          case Find1stGen(before, (v, coll), Find1stPred(before2, eqOperands @ Seq(a, b), rest)) =>
        }
      }
      //Non-deterministic matching without the List monad:
      val FindGen = SplitList(GenExtractor)
      val FindPred = SplitList(EqPred)
      qualifiers match {
        case FindGen(splits1) =>
          for {
            //Inefficient?
            (before, (v, coll), FindPred(splits2)) <- splits1
            (before2, eqOperands @ Seq(a, b), rest) <- splits2
          } yield {
            val MatchV = uniqueMatcherAndContexts(v)
            (for {
              side <- eqOperands
              MatchV(vAgain, proj) = side
              if guard(vAgain == v && eqOperands.size == 2) //DEBUG
              otherSide = (eqOperands.toSet - side).head //XXX could fail if the two sides are equal. Normalize away such things before!
            } yield (proj, otherSide)) match {
              case Seq((proj, otherSide)) => // Check there's a single match.
                import Lifting._
                //XXX finish reconstructing the target expression.
                (before, coll indexBy proj apply otherSide, before2, rest)
            }
          }
      }
      //This tries the sequence at different points. Must readd the head!
      qualifiers.tails collect {
        case Seq(Generator(v, coll), /*..., */ Predicate(Sym(Eq(a, b))))
        =>
          val MatchV = uniqueMatcherAndContexts(v)
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
