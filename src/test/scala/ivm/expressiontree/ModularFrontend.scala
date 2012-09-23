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

  import Lifting._
  //This macro produces the same code as above, after expansion. What is important is that this macro guarantees adequacy.
  println(Macros.wrap {
    pure(1) + 2
  })
  //This is accepted, although it is an exotic term:
  println(
    pure(pure(1).interpret() + 2)
  )
  //But this isn't:
  /*
  println(Macros.wrap {
    pure(1).interpret() + 2
  })
  */
}
class ModularFrontendExample extends Interpreted[BaseLangIntf with ScalaLangIntf, Int] {
  def apply(s: ThisLangIntf): s.Rep[Int] = {
    import s._
    pure(1) + 2
  }
}


// vim: set ts=8 sw=2 et:
