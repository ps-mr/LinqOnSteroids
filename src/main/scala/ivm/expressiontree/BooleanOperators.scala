package ivm.expressiontree


case class And(x: Exp[Boolean], y: Exp[Boolean]) extends BinaryOpSymmExp[Boolean, Boolean](x, y) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = And(x, y)
  def interpret() = x.interpret() && y.interpret()
}

case class Or(x: Exp[Boolean], y: Exp[Boolean]) extends BinaryOpSymmExp[Boolean, Boolean](x, y) {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = Or(x, y)
  def interpret() = x.interpret() || y.interpret()
}

case class Not(x: Exp[Boolean]) extends UnaryOpExp[Boolean, Boolean](x) {
  def copy(x: Exp[Boolean]) = Not(x)
  def interpret() = !x.interpret()
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