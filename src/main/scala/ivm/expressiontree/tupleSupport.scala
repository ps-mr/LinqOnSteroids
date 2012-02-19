package ivm.expressiontree

case class LiftTuple2[A, B](a: Exp[A], b: Exp[B]) extends Arity2OpExp[A, B, (A, B), LiftTuple2[A, B]](a, b) {
  def copy(a: Exp[A], b: Exp[B]) = LiftTuple2(a, b)
  def interpret() = (a.interpret(), b.interpret())
}

case class Tuple2Proj1[A, B](p: Exp[(A, B)]) extends UnaryOpExp[(A, B), A, Tuple2Proj1[A, B]](p) {
  def interpret() = p.interpret()._1
  def copy(p: Exp[(A, B)]) = Tuple2Proj1(p)
}

case class Tuple2Proj2[A, B](p: Exp[(A, B)]) extends UnaryOpExp[(A, B), B, Tuple2Proj2[A, B]](p) {
  def interpret() = p.interpret()._2
  def copy(p: Exp[(A, B)]) = Tuple2Proj2(p)
}
