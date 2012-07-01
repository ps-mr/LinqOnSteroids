package ivm
package optimization

import expressiontree._
import Lifting._
import OptimizationUtil._

/**
 * User: pgiarrusso
 * Date: 30/6/2012
 */

trait Fusion {
  private def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: Fun[T, U], g: Fun[U, V]): Exp[Traversable[V]] =
    coll.map(x => letExp(f(x))(g))

  //coll.map(f.f andThen g.f) //Old implementation, equivalent to inlining f(x) in the body of g. This might duplicate work!
  //coll.map(g.f andThen f.f) //Here the typechecker can reject this line.

  val mergeMaps: Exp[_] => Exp[_] = {
    case MapNode(MapNode(coll, f), g) =>
      //mergeMaps(coll.map(f2.f andThen f1.f))  //This line passes the typechecker happily, even if wrong. Hence let's
      //exploit parametricity, write a generic function which can be typechecked, and call it with Any, Any, Any:
      buildMergedMaps(coll, f, g)
    //Since inner nodes were already optimized, coll will not be a MapNode node, hence we needn't call mergeMaps on the
    //result.
    //coll map (x => g(f(x)))
    //coll.map(Fun(f.andThen(g))) //Demonstrate norm-by-eval.
    case e => e
  }

  private def buildMergedFlatMap1[T, U, V](coll: Exp[Traversable[T]], f: Fun[T, U], g: Fun[U, Traversable[V]]) =
    coll flatMap (x => letExp(f(x))(g))

  val mergeFlatMaps: Exp[_] => Exp[_] = {
    case FlatMap(MapNode(coll, f), g) =>
      //mergeFlatMaps(coll flatMap (x => letExp(f(x))(g)))
      mergeFlatMaps(buildMergedFlatMap1(coll, f, g))
      //XXX is the case below useful? Never tested. It's probably better to see what shortcut fusion would do instead.
    /*case MapNode(FlatMap(coll, f), g) =>
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(_ map g)))*/
    case e => e
  }

  //Express in the type system that transformations need to preserve typing:
  trait Transformer {
    def apply[T](e: Exp[T]): Exp[T]
  }

  val mergeMaps2: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case MapNode(MapNode(coll, f1), f2) =>
        mergeMaps2(buildMergedMaps(coll, f1, f2)).
          //Note the need for this cast.
          asInstanceOf[Exp[T]]
      case _ => e
    }
  }

  val mergeMaps3: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case m: MapNode[t, repr, u, that] =>
        Util.assertType[Exp[repr]](m.base)
        (m.base map m.f)(m.c)
      //Outdated comments:
      //m.base.map[u, that](m.f)(m.c) //does not work for the same reason - m.base is considered as having type Exp[Traversable[t]]. That's however because of my implicit conversion, which is
      //rather limited.
      //MapNode[t, repr, u, that](m.base, m.f)(m.c) //works
      //MapNode(m.base, m.f)(m.c) //works
      case _ => e
    }
  }

  val mergeFilters: Exp[_] => Exp[_] = {
    case e@Filter(col, f) =>
      stripViewUntyped(col) match {
        case Filter(col2, f2) =>
          mergeFilters(
            col2 withFilter {
              (x: Exp[_]) => And(f2(x), f(x))
            })
        case _ => e
      }
    case e => e
  }

  //Fuse multiple views
  val mergeViews: Exp[_] => Exp[_] = {
    case View(coll@View(_)) =>
      coll
    case e => e
  }

  /*
   * Consider:
   * coll1 flatMap (x1 => coll2 map (x2 => (x1, x2)) filter (pair => test(pair._1))
   * Conceptually, here the filter might be hoisted because it only depends on x1. But that's not visible unless we
   * merge the map and the filter, fuse their functions, and extract the filter again with transformedFilterToFilter.
   */
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    case Filter(MapNode(coll, mapFun), pred) => //This case should have higher priority, it seems more useful.
      coll flatMap Fun.makefun(letExp(mapFun.body)(Fun.makefun(if_#(pred.body)(Seq(pred.x)) else_# (Seq.empty), pred.x)), mapFun.x)
    //We preserve sharing here with letExp; currently, subsequent stages will do indiscriminate inlining and replicate the map, but
    //the inliner will later be improved.
    //This case, together with inlinng and transformedFilterToFilter, is equivalent to what Tillmann suggested and
    //then dismissed.
    /*
case MapNode(Filter(coll, pred @ FuncExpBody(test)), mapFun) =>
  //This transformation cancels with transformedFilterToFilter + flatMapToMap. Not sure if it's ever useful.
  //It could be useful as a separate stage, if this turns out to be a faster implementation.
  //After this optimization, we need to float the if_# to around the body of mapFun
  //coll flatMap (Fun.makefun(if_# (test)(Seq(pred.x)) else_# {Seq.empty}, pred.x) andThen mapFun)
  //Let's do this directly:
  coll flatMap Fun.makefun(
    if_# (test) {
      Seq(mapFun(pred.x))
    } else_# {
      Seq.empty
    }, pred.x)
    */
    case e => e
  }

  val transformedFilterToFilter: Exp[_] => Exp[_] = {
    //case FlatMap(coll, fmFun @ FuncExpBody(IfThenElse(test, thenBranch @ ExpSeq(Seq(element)), elseBranch @ ExpSeq(Seq())))) =>
    //coll filter
    case FlatMap(coll, fmFun@FuncExpBody(IfThenElse(test, thenBranch, elseBranch@ExpSeq(Seq())))) =>
      coll filter Fun.makefun(test, fmFun.x) flatMap Fun.makefun(thenBranch, fmFun.x)
    case e => e
  }
}
