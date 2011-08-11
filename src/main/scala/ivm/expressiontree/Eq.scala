package ivm.expressiontree

case class Eq[/*@specialized(Int, Boolean, Double)*/ T](x: Exp[T], y: Exp[T]) extends Exp[Boolean] {
  def children = Seq(x,y)
  def genericConstructor = (v) => Eq(v(0),v(1)) // why does this compile without cast?
  def interpret() = x.interpret().equals(y.interpret());
  //The implementation below could work better when specialization is active.
  // But apparently that annotation is not enough. Retry by specializing also Exp[T] for T = Int, Boolean, ...
  //def interpret() = x.interpret() == y.interpret();
}
