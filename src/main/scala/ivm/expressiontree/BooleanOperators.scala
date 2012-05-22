package ivm.expressiontree

import collection.mutable.{Set => MSet, LinkedHashSet}
import ivm.optimization.Optimization

object BooleanOperators {
  // convert formula to CNF using naive algorithm
  // contract: returns a set of clauses, each of which is a disjunction of literals
  def cnfInternal(in: Exp[Boolean]): MSet[Exp[Boolean]] = {
     in match {
       case And(x,y) =>
         cnfInternal(x) ++= cnfInternal(y)
       case Not(Not(x)) => cnfInternal(x)
       case Not(And(x, y)) => cnfInternal(Or(Not(x), Not(y)))
       case Not(Or(x, y)) => cnfInternal(And(Not(x), Not(y)))
       case Or(x, y) => {
         val cnfx = cnfInternal(x)
         val cnfy = cnfInternal(y)
         for (clauseX <- cnfx; clauseY <- cnfy)
           //Since both clauseX and clauseY are disjunction of literals, so is their disjunction, hence the result is a
           //correct CNF conversion.
           yield Or(clauseX, clauseY)
       }
       case _ => LinkedHashSet(in)
     }
  }
  def cnf(in: Exp[Boolean]): Set[Exp[Boolean]] = cnfInternal(Optimization.simplifyConditions(in)).toSet
}
