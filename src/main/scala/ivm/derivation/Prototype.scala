package ivm
package derivation

import optimization._
import TransformationCombinators._
import expressiontree._
import Lifting._

import collection.TraversableLike
import collection.generic.CanBuildFrom
import language.postfixOps

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */
trait Prototype {
  def isIncremental: PartialFunction[Exp[_], Boolean] = {
    case Sym(Var(_)) => true //This is a stub implementation - we should check for incremental collections instead.
  }
  //This is a top-down procedure.
  def derive2[T](e: Exp[T]): (Exp[T], Seq[Var]) = {
    case class VarDelta[T](v: Exp[T], delta: TypedVar[T])
    val baseCollections = e __find isIncremental
    val varDeltaPairs = baseCollections map (v => VarDelta(v, Fun.gensym()))
    val toDelta: Map[Exp[_], Var] = varDeltaPairs map {case VarDelta(v: Exp[_], delta: Var) => v -> delta} toMap;

    // Invariant: this should only be invoked when T is in fact a monoid/group. Otherwise, we need to be able to make it into a
    // group by adding negation.
    // For instance, we can't have map(T => Int): we need to use flatMap(T => Bag[Int]), where Bag[T] allows for negative
    // multiplicities.
    def topDownFiniteDiff[T]: Transformer[T] = Transformer[T] {
      case baseColl if isIncremental(baseColl) => toAtomImplicit(toDelta(baseColl))
      case Const(_) =>
        Seq.empty
      case Sym(FlatMap(base, f: FunSym[s, t])) => topDownFiniteDiff(base) flatMap f union
                                                  (base flatMap (x => topDownFiniteDiff(f(x)))) //XXX this looks expensive.
      // This would also mean that the graph changes when new elements are
      // added, which would be unacceptable. XXX No! That'd be the case if flatMap were run now, but it's just staged!
      // TODO: here and below, replace that with some other approach. Remember the idea of splitting these functions into
      //a function creating a data structure and a projection to what we need.
      //IDEA: we could have FlatMapMaintain, re-emit this node here and rely on CSE to have it built only once, and use
      //it to handle changes to the subcollections. Changes to base could still be handled using derivatives - which
      // would however need to update also that node.
      // It seems that the best solution might be to use a derivation-based approach only for first-order operators.

      case Sym(MapNode(base, f: FunSym[s, t])) => topDownFiniteDiff(base) map f union
                                                  (base map (x => topDownFiniteDiff(f(x)))) //XXX this looks expensive.
      //Same problems as above, plus the fact that `t` might not be a monoid/group.
      case Sym(Filter(base, p: FunSym[s, Boolean])) =>
        topDownFiniteDiff(base) filter p union
          (base filter (x => topDownFiniteDiff(p(x)))) //XXX again bad.
      case Sym(Union(a, b)) =>
        topDownFiniteDiff(a) union topDownFiniteDiff(b)
      //case Sym(Join(collA: Exp[Traversable[a]], collB: Exp[Traversable[b]], keyASel, keyBSel, resultSel)) =>
      case Sym(Join(collA, collB, keyASel, keyBSel, resultSel)) =>
        val diffA = topDownFiniteDiff(collA)
        val diffB = topDownFiniteDiff(collB)
        diffA.join(collB)(keyASel, keyBSel, resultSel) union
        collA.join(diffB)(keyASel, keyBSel, resultSel) union
        diffA.join(diffB)(keyASel, keyBSel, resultSel)
      case unhandledExp@Sym(unhandledDef) =>
        //Doesn't work - we need to use ++ only for collection children, not for all of them.
        //toAtomImplicit(unhandledDef.genericConstructor(unhandledDef.children map (child => child ++ topDownFiniteDiff(child)))) diff unhandledExp
        ???
    }
    (topDownFiniteDiff(e), varDeltaPairs map (_.delta))
  }

  def derive[T, U: TypeTag](e: Exp[T], BaseColl: Exp[Traversable[U]]): Exp[Traversable[U] => T] = {
    val deltaVVar = Fun.gensym[Traversable[U]]()
    val DeltaV = Sym(deltaVVar)
    def correctDependencies(a: Exp[_], b: Exp[_]) =
      (a isOrContains BaseColl) && !(a isOrContains DeltaV) &&
      (b isOrContains DeltaV) /*
      && !(b isOrContains BaseColl) //That's only required for self-maintainable views.
      */

    /*
    def topDownFiniteDiff[T]: Transformer[T] = Transformer[T] {
      case BaseColl => BaseColl union DeltaV
      case Sym(MapNode(base, f: FunSym[s, t])) => topDownFiniteDiff on base map f union
                                                  (base map (x => topDownFiniteDiff on f(x))) //crap!
      case Sym(Filter(base, p: FunSym[s, Boolean])) =>
        topDownFiniteDiff on base filter f
      case Sym(Union(a, b)) =>
        a union b
    }
    */

    def deriver[T] = Transformer[T] {
      case BaseColl =>
        //The interesting operator is not really union - it should be extended to handle also removals, but that's doable.
        BaseColl union DeltaV
        //Also, DeltaV might not be of the same type. The delta for a list has positions in it.
        //But for a Set or a Bag, no problem. For a Map, ?.
        //For a List[T], it's more a set of events like Add(pos: Int, el: T), or even Add(pos: Int, el: List[T]) (to
        // batch additions).
      //case Sym(MapNode(Sym(Union(BaseColl, DeltaV)), f)) => (BaseColl map f) union (DeltaV map f)
      //This should match more something like Union of f(BaseColl) and g(DeltaV, BaseColl). I guess more something
      // like:

      case Sym(MapNode(Sym(Union(a, b)), f)) if correctDependencies(a, b) =>
        (a map f) union (b map f)

      case Sym(FlatMap(Sym(Union(a, b)), f)) if correctDependencies(a, b) =>
        (BaseColl flatMap f) union (DeltaV flatMap f)
      case Sym(Filter(Sym(Union(a, b)), p)) if correctDependencies(a, b) =>
        (BaseColl filter p) union (DeltaV filter p)

      case Sym(Union(Sym(Union(BaseColl, DeltaV)), coll)) =>
        BaseColl union coll union DeltaV //Side condition: no ordering.
      case Sym(i: IndexBy[U, repr, k, that]) =>
        i match {
          case IndexBy(Sym(Union(BaseColl, DeltaV)), f) =>
            implicit val ctU: ClassTag[U] = i.classTagT
            (BaseColl indexBy f) union (DeltaV indexBy f)
        }
    }
    val res = Sym(new FunInterp(e transform (fromPoly(deriver | emptyTransform)), deltaVVar))
    res
  }

  //How to handle derivation of f(g(x)), say with f = sin?
  def topDownMathDerivative(Base: TypedVar[Int]): Exp[Int] => Exp[Int] = {
    case Sym(Plus(t1, t2)) => topDownMathDerivative(Base)(t1) + topDownMathDerivative(Base)(t2)
    case Sym(Times(t1, t2)) => topDownMathDerivative(Base)(t1) * t2 + t1 * topDownMathDerivative(Base)(t2)
    case Sym(Base) => 1
    case Sym(TypedVar(_)) => 0
    case Const(_) => 0
    case e => e
  }
}

//Prepare test cases
