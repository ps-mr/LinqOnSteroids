/*
class BugReport {
  trait Exp[+T]
  case class Foo[T](v: Exp[T]) extends Exp[T]

  def bugReports() {
    val conds: Set[Exp[Boolean]] = null

    //Both the following expressions must be present to trigger the bug. Commented out are variations I tried - when no
    // explanation is present, it means that either variant triggers the bug.

    conds.map {
      case Foo(l) => l //this must be l to trigger the bug
      case _ => null// Seq.empty
    }.fold(null /*Seq.empty*/)(_);

    conds.map {
      case Foo(l) => 1// null//l //this can be null, l, 1, i.e. whatever.
      case _ => null//Seq.empty
    }.fold(null /*Seq.empty*/) _
  }
}
*/
