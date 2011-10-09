package ivm.expressiontree

import ClassManifest._

case class IfInstanceOf[T,S](x: Exp[T])(implicit val cS: ClassManifest[S]) extends UnaryOpExp[T,Option[S]](x) {
  def copy(x: Exp[T]) = IfInstanceOf(x)
  def interpret() = {
    val v = x.interpret()
    if (v == null) None else {
      val mf = fromClass(v.getClass)
      if (mf <:< cS) Some(v.asInstanceOf[S]) else None
    }
  }
}