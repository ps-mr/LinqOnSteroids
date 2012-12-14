package ivm
package derivation

import optimization._
import TransformationCombinators._
import expressiontree._
import Lifting._
import collection.IterableLike

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */
trait Prototype {
  def derive[T, U: TypeTag](e: Exp[T], BaseColl: Exp[Traversable[U]]): Exp[T] = {
    val DeltaV = Sym(Fun.gensym[Traversable[U]]())
    val deriver = Transformer2 {
      case BaseColl =>
        //The interesting operator is not really union - it should be extended to handle also removals, but that's doable.
        BaseColl union DeltaV
        //Also, DeltaV might not be of the same type. The delta for a list has positions in it.
        //But for a Set or a Bag, no problem. For a Map, ?.
        //For a List[T], it's more a set of events like Add(pos: Int, el: T), or even Add(pos: Int, el: List[T]) (to
        // batch additions).
        //This should match more something like Union of f(BaseColl) and g(DeltaV).
      case Sym(MapNode(Sym(Union(BaseColl, DeltaV)), f)) => (BaseColl map f) union (DeltaV map f)
      case Sym(FlatMap(Sym(Union(BaseColl, DeltaV)), f)) => (BaseColl flatMap f) union (DeltaV flatMap f)
      case Sym(Filter(Sym(Union(BaseColl, DeltaV)), p)) => (BaseColl filter p) union (DeltaV filter p)
      case Sym(i: IndexBy[U, repr, k, that]) =>
        i match {
          case IndexBy(Sym(Union(BaseColl, DeltaV)), f) =>
            implicit val ctU: ClassTag[U] = i.classTagT
            (BaseColl indexBy f) union (DeltaV indexBy f)
        }
    }
    e transform (deriver | emptyTransform2)
  }
}

//Prepare test cases
