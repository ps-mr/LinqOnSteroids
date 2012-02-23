package ivm.expressiontree

import collection.mutable.{Set => MSet, LinkedHashSet}

// Warning: it is important that these operators are short-circuiting. Therefore, they cannot be expressed through
// CommOp like Plus and Times, and they are not commutative.
case class And(t1: Exp[Boolean], t2: Exp[Boolean]) extends Arity2OpSymmExp[Boolean, Boolean, And] {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = And(x, y)
  def interpret() = t1.interpret() && t2.interpret()
}

case class Or(t1: Exp[Boolean], t2: Exp[Boolean]) extends Arity2OpSymmExp[Boolean, Boolean, Or] {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = Or(x, y)
  def interpret() = t1.interpret() || t2.interpret()
}

case class Not(t1: Exp[Boolean]) extends Arity1OpExpTrait[Boolean, Boolean, Not] {
  def copy(x: Exp[Boolean]) = Not(x)
  def interpret() = !t1.interpret()
}

case class IfThenElse[T](cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) extends Arity3OpExp[Boolean, T, T, T, IfThenElse[T]](cond, thenBody, elseBody) {
  def interpret() = if (cond.interpret()) thenBody.interpret() else elseBody.interpret()
  def copy(cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) = IfThenElse(cond, thenBody, elseBody)
}

case class OptionGetOrElse[T](opt: Exp[Option[T]], default: Exp[T]) extends Arity2OpExp[Option[T], T, T, OptionGetOrElse[T]](opt, default) {
  def interpret() = opt.interpret().getOrElse(default.interpret())
  def copy(opt: Exp[Option[T]], default: Exp[T]) = OptionGetOrElse(opt, default)
}

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
  def cnf(in: Exp[Boolean]): Set[Exp[Boolean]] = cnfInternal(in).toSet
}
