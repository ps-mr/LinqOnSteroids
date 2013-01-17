/**
 * Ideas about incremental view maintenance
 * written down in Scala
 * a more precise language than English
 */

package bin
object static {


import ivm.expressiontree._

// Input: AST of view
// Output: AST expressing data dependency

// Changes in a set

sealed abstract class Situation[+A]
case class HasChange[A](toDel: Exp[Set[A]], toIns: Exp[Set[A]]) extends Situation[A]
case class RefSource[A](source: Const[Set[A]]) extends Situation[A]
case object Impossible extends Situation[Nothing]

def incre[A](ast: Exp[Set[A]]): Situation[A]
= ast match {
  case Sym(MapNode(base, f)) => incre(base.asInstanceOf[Exp[Set[A]]]) match {
    case Impossible => Impossible
    // What to do if it's a reference to a source?
  }
  case c@Const(_) => RefSource(c)
  case _ => Impossible
}


}


/**
 * Trial run
 */
object trial {

import squopt.imports._
import static._

val ee = Set(1,2,3,4,5).asSquopt
val e0 = ee.filter{_ <= 4}.map{_ + 2}
val e1 = ee.filter{_ <= 3}.flatMap{x => List(x, -x)}.map{_ * 3}


}

