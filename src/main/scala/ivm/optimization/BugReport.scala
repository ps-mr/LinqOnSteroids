package ivm
package optimization

import expressiontree._
import scala.collection.Set


/**
 * User: pgiarrusso
 * Date: 22/2/2012
 */

/*
class BugReport {
  def bugReports_v2[T](e: Exp[T]) {
    val conds: Set[Exp[Boolean]] = null

    conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
      case _ => Set.empty[Exp[Any]]
    }.fold(Set.empty[Exp[Any]])(_);

    conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
      case _ => Set.empty[Exp[Any]]
    }.fold(Set.empty[Exp[Any]]) _
    //}
    /*conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet
      case _ => Set.empty
    }.fold(Set.empty[Exp[Any]]) _*/
  }

  /*def bugReports[T](e: Exp[T]) {
    e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        val conds: Set[Exp[Boolean]] = null

        conds.map {
          case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
          case _ => Set.empty[Exp[Any]]
        }.fold(Set.empty[Exp[Any]])(_)


        //Works:
        conds.map {
          case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
          case _ => Set.empty[Exp[Any]]
        }.fold(Set.empty[Exp[Any]]) _
    }
  }*/
}
*/
