package ivm.expressiontree

trait LangIntf {
  type Rep[+T]
}
/*
object ModularFrontendDefsObj extends ModularFrontendDefs

import ModularFrontendDefsObj._
*/

/* Compared to the example below, the LangIntf classes can be taken out and LangIntf can just become a type parameter.*/
/*trait ScalaIntf {
  this: Interface =>
  type LangIntf <: ScalaLangIntf*/
  trait ScalaLangIntf {
    this: LangIntf =>
    //Why not an implicit abstract class? Ah I see.
    implicit def toIntOps(v: Rep[Int]): IntOps
    abstract class IntOps(v: Rep[Int]) {
      def +(that: Rep[Int]): Rep[Int]
    }
  }
//}

/*trait Interface {
  type LangIntf <: BaseLangIntf
  trait BaseLangIntf {
  */
  trait BaseLangIntf extends LangIntf {
    type Rep[+T]
    implicit def pure[T](t: T): Rep[T]
  }
/*}

trait MyIntf extends Interface with ScalaIntf {
  type LangIntf <: BaseLangIntf with ScalaLangIntf
}
*/

trait Interpreted[Sym <: LangIntf, Res] {
  type ThisLangIntf = Sym
  def apply(s: ThisLangIntf): s.Rep[Res]
}
object ModularFrontendExample extends scala.App {
  println(new Interpreted[BaseLangIntf with ScalaLangIntf, Int] {
    def apply(s: ThisLangIntf): s.Rep[Int] = {
      import s._
      pure(1) + 2
    }
  })
}
class ModularFrontendExample extends Interpreted[BaseLangIntf with ScalaLangIntf, Int] {
  def apply(s: ThisLangIntf): s.Rep[Int] = {
    import s._
    pure(1) + 2
  }
}


// vim: set ts=8 sw=2 et:
