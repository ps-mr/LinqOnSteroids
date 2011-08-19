package ivm.expressiontree


case class And(x: Exp[Boolean], y: Exp[Boolean]) extends Exp[Boolean] {
  def children = Seq(x,y)
  def genericConstructor = (v) => And(v(0).asInstanceOf[Exp[Boolean]],v(1).asInstanceOf[Exp[Boolean]])
  def interpret() = x.interpret() && y.interpret();
}

case class Or(x: Exp[Boolean], y: Exp[Boolean]) extends Exp[Boolean] {
  def children = Seq(x,y)
  def genericConstructor = (v) => Or(v(0).asInstanceOf[Exp[Boolean]],v(1).asInstanceOf[Exp[Boolean]])
  def interpret() = x.interpret() || y.interpret();
}

case class Not(x: Exp[Boolean]) extends Exp[Boolean] {
  def children = Seq(x)
  def genericConstructor = (v) => Not(v(0).asInstanceOf[Exp[Boolean]])
  def interpret() = !x.interpret();
}