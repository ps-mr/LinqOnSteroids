package ivm.expressiontree

/*
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
*/

/*case class LiftTuple3[A, B, C](a: Exp[A], b: Exp[B], c: Exp[C]) extends Arity3OpExp[A, B, C, (A, B, C), LiftTuple3[A, B, C]](a, b, c) {
  def copy(a: Exp[A], b: Exp[B], c: Exp[C]) = LiftTuple3(a, b, c)
  def interpret() = (a.interpret(), b.interpret(), c.interpret())
}

case class Tuple3Proj1[A, B, C](p: Exp[(A, B, C)]) extends UnaryOpExp[(A, B, C), A, Tuple3Proj1[A, B, C]](p) {
  def interpret() = p.interpret()._1
  def copy(p: Exp[(A, B, C)]) = Tuple3Proj1(p)
}
*/
