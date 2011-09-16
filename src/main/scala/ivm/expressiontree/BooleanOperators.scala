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
  def cnf(in: Exp[Boolean]) : Set[Exp[Boolean]] = {
     in match {
       case And(x,y) => cnf(x) ++ cnf(y)
       case Not(Not(x)) => cnf(x)
       case Not(And(x,y)) => cnf(Or(Not(x), Not(y)))
       case Not(Or(x,y)) => cnf(And(Not(x),Not(y)))
       case Or(x,y) => {
         val cnfx = cnf(x)
         val cnfy = cnf(y)
         if ((cnfx.size == 1) && (cnfy.size == 1)) return Set(Or(cnfx.head, cnfy.head))
         val (c1,c2) = if (cnfx.size >1) (cnfy,cnfx) else (cnfx,cnfy)
         val c1factors = c1.fold( Const(true))( (a,b) => And(a,b))
         cnf(c2.map( (z) => Or(z,c1factors) ).fold( Const(true))( (a,b) => And(a,b)))
       }
       case _ => Set(in)

     }
  }
}