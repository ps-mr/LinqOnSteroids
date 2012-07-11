package ivm.expressiontree

// Warning: it is important that these operators are short-circuiting. Therefore, they cannot be expressed through
// CommOp like Plus and Times, and they are not commutative.
case class And(t1: Exp[Boolean], t2: Exp[Boolean]) extends Arity2OpSymmExp[Boolean, Boolean, And] with InfixPrinting {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = And(x, y)
  def interpret() = t1.interpret() && t2.interpret()
  def operator = "&&"
}

case class Or(t1: Exp[Boolean], t2: Exp[Boolean]) extends Arity2OpSymmExp[Boolean, Boolean, Or] with InfixPrinting {
  def copy(x: Exp[Boolean], y: Exp[Boolean]) = Or(x, y)
  def interpret() = t1.interpret() || t2.interpret()
  def operator = "&&"
}

case class Not(t1: Exp[Boolean]) extends Arity1OpExpTrait[Boolean, Boolean, Not] with PrefixPrinting {
  def copy(x: Exp[Boolean]) = Not(x)
  def interpret() = !t1.interpret()
  def prefix = "!"
}

case class IfThenElse[T](cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) extends Arity3OpExp[Boolean, T, T, T, IfThenElse[T]](cond, thenBody, elseBody) with FormatPrinting {
  def interpret() = if (cond.interpret()) thenBody.interpret() else elseBody.interpret()
  def copy(cond: Exp[Boolean], thenBody: Exp[T], elseBody: Exp[T]) = IfThenElse(cond, thenBody, elseBody)
  def formatStr = "if (%s) {%s} else {%s}"
}

case class OptionGetOrElse[T](opt: Exp[Option[T]], default: Exp[T]) extends Arity2OpExp[Option[T], T, T, OptionGetOrElse[T]](opt, default) with InfixPrinting {
  def interpret() = opt.interpret().getOrElse(default.interpret())
  def copy(opt: Exp[Option[T]], default: Exp[T]) = OptionGetOrElse(opt, default)
  def operator = "getOrElse"
}
