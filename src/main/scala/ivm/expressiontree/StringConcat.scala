package ivm.expressiontree

case class StringConcat(x: Exp[String], y: Exp[String]) extends Exp[String] {
  def interpret() = x.interpret() + y.interpret()
  def children = Seq(x,y)
  def genericConstructor = (v) => StringConcat(v(0).asInstanceOf[Exp[String]], 
                                               v(1).asInstanceOf[Exp[String]])
}
