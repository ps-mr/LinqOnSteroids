class BugReport {
  trait Exp[+T] {
    //def find(filter: PartialFunction[Exp[_], Boolean]): Seq[Exp[_]] = null
  }
  case class Foo[T](v: Exp[T]) extends Exp[T]

  def bugReports() {
    val conds: Set[Exp[Boolean]] = null

    //Both the following expressions must be present to trigger the bug.

    conds.map {
      case /*eq @ */Foo(l) => l//.find {case _ => true}
      case _ => Seq.empty
    }.fold(Seq.empty)(_);

    conds.map {
      case /*eq @ */Foo(l) => l//.find {case _ => true}
      case _ => Seq.empty
    }.fold(Seq.empty) _
  }
}
