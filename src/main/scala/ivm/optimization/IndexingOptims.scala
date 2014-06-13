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
    qualifiers.tails collect {
      case Seq(Generator(v, coll), /*..., */ Predicate(e))
        if e isOrContains v
      =>
        val F = uniqueFinderAndContexts(comparer(v))
        //Look for (v == [ ]) or ([ ] == v), returns the content of the hole if found.
        val getEqSides = extractor(Eq.unapply[Any] _) flatMap { case (x, y) =>
          val eqOperands = Set(x, y)
          if (eqOperands contains v)
            Some((eqOperands - v).head)
          else
            None
        }
        val G = uniqueFinderAndContexts((toExtractor[Exp[Any], Exp[Any]] { case Sym(Eq(`v`, u)) => u; case Sym(Eq(u, `v`)) => u }))
        e match {
          case G(matches) =>
            for { (vOccurs, ctx) <- matches } {
              import Lifting._
              coll indexBy ctx
            }
          case _ =>
        }
    }
  }
}
