package ivm
package optimization

import expressiontree._
import Lifting._
import optimization.OptimizationUtil.FuncExpBody

/**
 * User: pgiarrusso
 * Date: 3/7/2012
 */

object TransformationExperiments {
  /*
  private def removeIdentityMaps[T](e: Exp[T]): Exp[T] =
    e match {
      //Alternative 1 - a cast is required:
      case MapNode(col: Exp[_ /*T*/], FuncExpIdentity()) =>
        col.asInstanceOf[Exp[T]]
      //Alternative 2 - causes a warning, but works and is more elegant:
      case MapNode(col: Exp[T], FuncExpIdentity()) =>
        col
      //Possibility 2 is what is used in the Scala-virtualized tutorial.
      case e => e
    }
  */

  /*val betaReduction: Exp[_] => Exp[_] = {
    case a: App[t, u] => a.f(a.t)
    case ExpSelection(arity, selected, e: ExpProduct) => e.metaProductElement(selected - 1)
    case e => e
  }*/

  //val betaReduction: PartialFunction[Exp[_], Exp[_]] = {
    //case a: App[s, t] => a.f(a.t) //causes a warning
  //}
  val mergeFlatMaps: Exp[Any] => Exp[Any] = {
    case Sym(FlatMap(Sym(MapNode(coll, f)), g)) =>
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(g))) //The body is not really typechecked, unlike in buildMergedFlatMap1.
    //XXX is the case below useful? Never tested. It's probably better to see what shortcut fusion would do instead.
    case Sym(MapNode(Sym(FlatMap(coll, f)), g)) =>
      mergeFlatMaps(coll flatMap (x => letExp(f(x))(_ map g)))
    case e => e
  }

  def buildMergedMaps[T, U, V](coll: Exp[Traversable[T]], f: FunSym[T, U], g: FunSym[U, V]): Exp[Traversable[V]] =
    coll.map(f.f andThen g.f) //Old implementation, equivalent to inlining f(x) in the body of g. This might duplicate work!
  //OptimizationTransforms.buildMergedMaps avoids that problem.

  //Express in the type system that transformations need to preserve typing:
  trait Transformer {
    def apply[T](e: Exp[T]): Exp[T]
  }

  trait TransformerOpt {
    def apply[T](e: Exp[T]): Option[Exp[T]]
  }

  val mergeMaps2: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case Sym(MapNode(Sym(MapNode(coll, f1)), f2)) =>
        mergeMaps2(buildMergedMaps(coll, f1, f2)).
          //Note the need for this cast.
          asInstanceOf[Exp[T]]
      case _ => e
    }
  }

  val mergeMaps3: Transformer = new Transformer {
    def apply[T](e: Exp[T]) = e match {
      case Sym(m: MapNode[t, repr, u, that]) =>
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

  //Bug-triggering variant of Inlining.usesArgAtMostOnce
  /*@tailrec private def usesArgAtMostOnce[S, T](f: Fun[S, T], v: Exp[_]): Boolean = {
    f match {
      case FuncExpBody(FlatMap(ExpSeq(Seq(v2)), g)) if !v2.isOrContains(v) =>
        usesArgAtMostOnce(g, v)
      case FuncExpBody(FlatMap(baseUsingV, g)) =>
        val occurrences = baseUsingV.findTotFun(_ == v)
        occurrences.length == 1 //gives spurious error
        //false
      case _ => false
    }
  }*/

  val mergeMaps: PartialFunction[Exp[_], Exp[_]] = {
    case Sym(MapNode(Sym(m@MapNode(coll, f)), g)) =>
      //coll.map(g.f andThen f.f)  //This line passes the typechecker happily, even if wrong. Hence the actual code
      //exploits parametricity: it generates the new body in a generic function which can be typechecked, and calls it
      //with Any, Any, Any as type parameters since that's what type inference will deduce:
      //  buildMergedMaps(coll, f, g)
      //coll map (x => g(f(x))) //Simplest syntax
      (coll map (f andThen g))(m.c) //Demonstrate norm-by-eval.
  }
  val mergeFilterWithMap: Exp[_] => Exp[_] = {
    //This case, together with inlinng and transformedFilterToFilter, is equivalent to what Tillmann suggested and
    //then dismissed.

    case Sym(MapNode(Sym(Filter(coll, FunSym(pred @ FuncExpBody(test)))), mapFun)) =>
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
        }, pred.x).f
  }
  /*val mapToFlatMap: Exp[_] => Exp[_] = {
    case Call2(OptionOps.OptionMapId, _, c: Exp[Option[t]], f: Fun[_, u]) =>
      c flatMap Fun.makefun(Some(f.body), f.x)
  }*/
  val mergeMapsTyped = new TransformerOpt {
    //import optimization.&
    def apply[T](e: Exp[T]) = e match {
      //case (m: MapNode[t, repr, u, that]) & MapNode(c, base) => //doesn't refine the type of c and base.
      case Sym(m: MapNode[t, repr, u, that]) => //T = that
        Util.assertType[Exp[repr]](m.base)
        val ret1 = (m.base map m.f)(m.c)
        Util.assertType[Exp[that]](ret1)
        Util.assertType[Exp[T]](ret1)

        m.base match {
          case Sym(m2: MapNode[t2, repr2, u2, that2]) =>
            Some((expToTraversableLikeOps[t /*u2*/, /*that2*/ repr]((m2.base map m2.f)(m2.c)) map m.f)(m.c)) //works
          case _ => None
        }
      case _ => None
    }
  }
  //This just doesn't work, the type of m.base is Exp[repr with Traversable[t]].
  /*def mergeMapsPartial[T]: PartialFunction[Exp[T], Exp[T]] = ({
    //case (m: MapNode[t, repr, u, that]) & MapNode(c, base) => //doesn't refine the type of c and base.
    case m: MapNode[t, repr, u, that] => //T = that
      Util.assertType[Exp[repr]](m.base)
      val ret1 = (m.base map m.f)(m.c)
      Util.assertType[Exp[that]](ret1)
      Util.assertType[Exp[T]](ret1)
      m.base
  }: PartialFunction[Exp[T], Exp[T]]).andThen[Exp[T]] {
    case m2: MapNode[t2, repr2, u2, that2] =>
      (expToTraversableLikeOps[t /*u2*/, /*that2*/ repr]((m2.base map m2.f)(m2.c)) map m.f)(m.c)
  }*/
}
