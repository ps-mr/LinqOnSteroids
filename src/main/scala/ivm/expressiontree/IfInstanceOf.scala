package ivm.expressiontree

import ClassManifest._

case class IfInstanceOf[T,S](x: Exp[T])(implicit val cS: ClassManifest[S]) extends UnaryOpExp[T,Option[S]](x) {
  def copy(x: Exp[T]) = IfInstanceOf(x)
  def interpret() = {
    val v = x.interpret()
    if (fromClass(v.getClass) <:< cS) Some(v.asInstanceOf[S]) else None
  }
}