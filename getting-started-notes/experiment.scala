/**
 * Ideas about incremental view maintenance
 * written down in Scala
 * a more precise language than English
 *
 * Yufei Cai
 * 18.01.13
 */

package bin
object static {


import ivm.expressiontree._

// Changes in a set

case class DSet[A](toDel: Exp[Set[A]], toIns: Exp[Set[A]])

// A triple consisting of a source collection and what are but symbolic
// dummies of the change it may generate.
//
// The dummy change sets can be anything non-null of type Exp[Set[A]].
// Not sure how one uses the implicit parameters ClassTag and TypeTag
// but certain that I will not, they are nullified.
//
// Verified on 18.01.13 that consecutive calls to
//
//     System.identityHashCode(Const(null))
//
// returns distinct results.

class Source[A](origin: Const[Set[A]])
extends DSet[A](Const(null)(null, null), Const(null)(null, null))

// Defining a new suite of map/flatmap/filter nodes just to keep track of
// data flow is wasteful of time, but reusing Exp[_] puts us in the middle
// of conflicting concerns, one symptom of whose is the IdentityHashMap
// below with which we keep track of dummy deltas that are but links to
// source collections that are supposed to generate them.
//
// Caution: SourceMap assumes Const nodes of different identity hash codes
// to contain different source collections.

class SourceMap[A]
extends java.util.IdentityHashMap[Exp[Set[A]], Source[A]] {

  def getSource(ref: Exp[Set[A]]): Option[Source[A]] = {
    val source: Source[A] = super.get(ref)
    if (source == null) None
    else               Some(source)
  }

  def findOrCreateSource(origin: Const[Set[A]]): Source[A]
  = getSource(origin) match {
    case Some(source) => source
    case None => {
      val source = new Source[A](origin)

      // Following assertions ensure that a clash of identity hash codes
      // crashes the system right here right now and not somewhere else
      // sometime later.

      assert(null == super.put(origin, source))
      assert(null == super.put(source.toDel, source))
      assert(null == super.put(source.toIns, source))
      source
    }
  }

}

// Much-used FunSym

def notInside[A](here: Exp[Set[A]])
: FunSym[Any, Boolean] = sys error "TODO"

// Input: AST of view
// Output: AST expressing data dependency

// TODO: It appears we need multisets after all!

/*

def incre[A](ast: Exp[Set[A]], rememberTo: SourceMap[A]): Option[DSet[A]]
= ast match {
  case Sym(MapNode(base, f))
  => incre(base.asInstanceOf[Exp[Set[A]]], rememberTo) match {
    case Some(dset) => {
      val mappedToDel: Exp[Set[A]] = Sym(MapNode(dset.toDel, f))
      val mappedToIns: Exp[Set[A]] = Sym(MapNode(dset.toIns, f))
      val filteredToDel = Sym(Filter(mappedToDel, notInside(mappedToIns)))
      Some(DSet(filteredToDel, mappedToIns))
    }
    case None => None
  }
  case c@Const(_) => Some(rememberTo.findOrCreateSource(c))
  case _ => None
}

*/

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

