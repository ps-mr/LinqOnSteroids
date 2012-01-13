package ivm.expressiontree


case class And(override val t1: Exp[Boolean], override val t2: Exp[Boolean]) extends CommOp[Boolean](t1, t2)(_ && _) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = And(x, y)
}

case class Or(override val t1: Exp[Boolean], override val t2: Exp[Boolean]) extends CommOp[Boolean](t1, t2)(_ || _) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = Or(x, y)
}

case class Not(t1: Exp[Boolean]) extends UnaryOpExpTrait[Boolean, Boolean] {
  def copy(x: Exp[Boolean]) = Not(x)
  def interpret() = !t1.interpret()
}

object BooleanOperators {
  // convert formula to CNF using naive algorithm
  // contract: returns a set of clauses, each of which is a disjunction of literals
  def cnf(in: Exp[Boolean]): Set[Exp[Boolean]] = {
     in match {
       case And(x,y) => cnf(x) ++ cnf(y)
       case Not(Not(x)) => cnf(x)
       case Not(And(x,y)) => cnf(Or(Not(x), Not(y)))
       case Not(Or(x,y)) => cnf(And(Not(x), Not(y)))
       case Or(x,y) => {
         val cnfx = cnf(x)
         val cnfy = cnf(y)
         for (clauseX <- cnfx; clauseY <- cnfy)
           //Since both clauseX and clauseY are disjunction of literals, so is their disjunction, hence the result is a
           //correct CNF conversion.
           yield Or(clauseX, clauseY)
       }
       case _ => Set(in)
     }
  }
}