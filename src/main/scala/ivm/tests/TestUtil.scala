package ivm
package tests

import expressiontree._

trait TestUtil {
  def showExpNoVal[T](t: Exp[T], message: String = "") {
    print("\nQuery name: %s\n *\tstructure:\n\t%s\n" format(message, t))
  }

  def showExp[T](t: Exp[T], message: String = "") {
    showExpNoVal(t, message)
    print(" *\tvalue:\n\t%s\n\n" format t.interpret())
  }

  def show(name: String, v: Any) {
    print(name + ": ")
    println(v)
  }

  /*
  def showInterp(name: String, v: Exp[_]) {
    show(name, v)
    show(name + ".interpret()", v.interpret())
  }
  */

  def showInterp(name: String, v: Exp[_]) {
    showExp(v, name)
  }
}
