package ivm.expressiontree

/*
trait MyIntf extends Interface with ScalaIntf {
  type LangIntf <: BaseLangIntf with ScalaLangIntf
}
*/

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
