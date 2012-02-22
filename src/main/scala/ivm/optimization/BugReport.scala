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
  /*
  def bugReports_v2[T](e: Exp[T]) {
    val conds: Set[Exp[Boolean]] = null

    //Gives an error:
    /*conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
      case _ => Set.empty[Exp[Any]]
    }.fold(Set.empty[Exp[Any]])(_)*/

    //Crashes compiler:
    /*
    conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
      case _ => Set.empty[Exp[Any]]
    }.fold(Set.empty[Exp[Any]]) _
    */
    //}
    conds.map {
      case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet
      case _ => Set.empty
    }.fold(Set.empty[Exp[Any]]) _

  }
  */

  def bugReports[T](e: Exp[T]) {
    e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>

        /*def par2[A, B, C](f: (A, B) => C): ((A, B), (A, B)) => (C, C) = { case ((a1, b1), (a2, b2)) => (f(a1, b1), f(a2, b2))}
        def union[T] = (_: Seq[T]) union (_: Seq[T])
 def union2[T] = (_: Set[T]) union (_: Set[T])*/

        val conds: Set[Exp[Boolean]] = null

        /*conds.map {
         case eq @ Eq(l, r) => l.find {case Var(_) => true}
         case _ => Set.empty[Exp[_]]
       }.fold((Set.empty[Exp[_]], Set.empty[Exp[_]]))(_)*/
        /*
        conds.map {
          case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[_]]
          case _ => Set.empty[Exp[_]]
        }.fold(Set.empty[Exp[_]])(_)
        */
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
  }

  /*
  def lookupEq[T](e: Exp[T]): Set[Exp[_]] = {
    e match {
      case Filter(c: Exp[Traversable[_ /*t*/]], f: FuncExp[t, _ /*Boolean*/]) =>
        def par2[A, B, C](f: (A, B) => C): ((A, B), (A, B)) => (C, C) = { case ((a1, b1), (a2, b2)) => (f(a1, b1), f(a2, b2))}
        def union[T] = (_: Seq[T]) union (_: Seq[T])
        def union2[T] = (_: Set[T]) union (_: Set[T])

        val conds: Set[Exp[Boolean]] = BooleanOperators.cnf(f.body)

        conds.map {
          case eq @ Eq(l, r) => l.find {case Var(_) => true}.toSet[Exp[Any]]
          case _ => Set.empty[Exp[Any]]
        }.fold(Set.empty[Exp[Any]])(_)

        conds.map {
          case eq @ Eq(l, r) => ((l.find {case Var(_) => true}).toSet, Set[Exp[_]](eq))
          case _ => (Set.empty[Exp[_]], Set.empty[Exp[_]])
        }.fold((Set.empty[Exp[_]], Set.empty[Exp[_]]))(par2(union2))
        /*
        //
        [error]  found   : [T](scala.collection.Set[T], scala.collection.Set[T]) => scala.collection.Set[T]
[error]  required: (Iterable[ivm.expressiontree.Exp[_]] with Int with ivm.expressiontree.Exp[Any] => Any, scala.collection.Set[ivm.expressiontree.Exp[_]]) => scala.collection.Set[ivm.expressiontree.Exp[_]]
[error]         }.fold((Set.empty[Exp[_]], Set.empty[Exp[_]]))(par2(union2))
[error]                                                             ^

        */
        /*
        import scalaz._
        import Scalaz._
        */
        def par[A, B, C](f: (A, B) => C): ((A, A), (B, B)) => (C, C) = { case ((a1, a2), (b1, b2)) => (f(a1, b1), f(a2, b2))}

        //Using Sets here directly is close to impossible, due to the number of wildcards and the invariance of Set.
        conds.map {
          case eq @ Eq(l, r) => (l.find {case Var(_) => true}, Seq[Exp[_]](eq))
          case _ => (Seq.empty, Seq.empty)
        }.fold((Seq.empty, Seq.empty))(par2(union))

        conds.map {
          case eq @ Eq(l, r) => l.find {case Var(_) => true}
          case _ => Seq.empty
        }.fold(Seq.empty)(_ union _).toSet
      case FlatMap(c, f) =>
        //Add to this the variables on which the free vars of subexp depend? No. Add all free variables bound in the location.
        lookupEq(c) union lookupEq(f.body)
      case MapOp(c, f) =>
        lookupEq(c) union lookupEq(f.body)
      case _ => Set.empty
    }
  }
  */
}
*/
