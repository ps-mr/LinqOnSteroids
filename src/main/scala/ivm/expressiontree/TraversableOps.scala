package ivm.expressiontree

import collection.generic.CanBuildFrom
import collection.{TraversableView, TraversableViewLike, TraversableLike, GenTraversableOnce, mutable}
import ivm.collections.TypeMapping

trait TraversableOps {
  this: BaseExps with BaseTypesOps =>
  def newFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr],
                                                                             f: FuncExp[T, Boolean]) =
    Filter(base, f)
  def newWithFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr],
                                                                             f: FuncExp[T, Boolean]) =
    newFilter(View(base), f)
  def newMapOp[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr],
                                                                                                   f: FuncExp[T, U])
                                                                                                  (implicit c: CanBuildFrom[Repr, U, That]) =
    MapOp(base, f)
  def newFlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr], f: FuncExp[T, TraversableOnce[U]])
                                        (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    FlatMap(base, f)

  def newUnion[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U >: T, That <: Traversable[U]](base: Exp[Repr with Traversable[T]], that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    new Union(base, that)


  /* Lift faithfully the FilterMonadic trait except foreach and withFilter, since we have a special lifting for it.
   * This trait is used both for concrete collections of type Repr <: FilterMonadic[T, Repr].
   */
  trait FilterMonadicOpsLike[T, Repr <: Traversable[T] with TraversableLike[T, Repr]] {
    val t: Exp[Repr]
    def map[U, That <: Traversable[U]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newMapOp(this.t, FuncExp(f))
    def map2[U, That <: Traversable[U]](f: T => U)(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newMapOp(this.t, FuncExp(f: Exp[T => U]))
    def flatMap[U, That <: Traversable[U]](f: Exp[T] => Exp[TraversableOnce[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newFlatMap(this.t, FuncExp(f))
  }

  case class GroupBy[T, Repr <: TraversableLike[T, Repr], K](base: Exp[Repr], f: Exp[T => K]) extends BinaryOpExp[Repr,
    T => K, Map[K, Repr], GroupBy[T, Repr, K]](base, f) {
    override def interpret() = base.interpret() groupBy f.interpret()
    override def copy(base: Exp[Repr], f: Exp[T => K]) = GroupBy(base, f)
  }


  case class Join[T, Repr <: TraversableLike[T, Repr], S, TKey, TResult, That](colouter: Exp[Repr],
                                                                               colinner: Exp[Traversable[S]],
                                                                               outerKeySelector: FuncExp[T, TKey],
                                                                               innerKeySelector: FuncExp[S, TKey],
                                                                               resultSelector: FuncExp[(T, S), TResult])
                                                                              (implicit cbf: CanBuildFrom[Repr, TResult, That]) extends
  QuinaryOp[Exp[Repr],
    Exp[Traversable[S]],
    FuncExp[T, TKey], FuncExp[S, TKey], FuncExp[(T, S), TResult],
    That](colouter, colinner, outerKeySelector, innerKeySelector, resultSelector) {
    override def copy(colouter: Exp[Repr],
                      colinner: Exp[Traversable[S]],
                      outerKeySelector: FuncExp[T, TKey],
                      innerKeySelector: FuncExp[S, TKey],
                      resultSelector: FuncExp[(T, S), TResult]) = Join(colouter, colinner, outerKeySelector, innerKeySelector, resultSelector)

    override def interpret() = {
      // naive hash join algorithm
      val ci: Traversable[S] = colinner.interpret()
      val co: Repr = colouter.interpret()
      val builder = cbf(co)
      //XXX: this is non-order-preserving, and might be suboptimal.
      // In databases, we build the temporary index on the smaller relation, so that the index fits more easily in
      // memory. This concern seems not directly relevant here; what matters here is only whether insertions or lookups in a
      // hash-map are more expensive. OTOH, it is probably important that the temporary index fits at least in the L2 cache,
      // so we should index again on the smaller relation!
      if (ci.size > co.size) {
        val map  = ci.groupBy(innerKeySelector.interpret()) //Cost O(|ci|) hash-map insertions
        for (c <- co; d <- map(outerKeySelector.interpret()(c))) //Cost O(|co|) hash-map lookups
          builder += resultSelector.interpret()(c, d)
      } else {
        val map  = co.groupBy(outerKeySelector.interpret())
        for (c <- ci; d <- map(innerKeySelector.interpret()(c)))
          builder += resultSelector.interpret()(d, c)
      }
      builder.result()
    }
  }

  //This is just an interface for documentation purposes.
  trait WithFilterable[T, Repr] {
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]]
  }

  trait WithFilterImpl[T, This <: Traversable[T] with TraversableLike[T, Repr], Repr <: Traversable[T] with TraversableLike[T, Repr]] extends WithFilterable[T, Repr] {
    self: FilterMonadicOpsLike[T, Repr] =>
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]] =
      newWithFilter(this.t, FuncExp(f))
  }

  def groupBySelImpl[T, Repr <: Traversable[T] with
    TraversableLike[T, Repr], K, Rest, That <: Traversable[Rest]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                                                  g: Exp[T] => Exp[Rest])(
    implicit c: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]]

  trait TraversableLikeOps[T, Coll[X] <: Traversable[X] with TraversableLike[X, Coll[X]], Repr <: Traversable[T] with TraversableLike[T, Repr] with Coll[T]] extends FilterMonadicOpsLike[T, Repr] {
    def collect[U, That <: Traversable[U]](f: Exp[T] => Exp[Option[U]])
                                          (implicit c: CanBuildFrom[TraversableView[T, Repr], U, That]): Exp[That] = {
      newMapOp(newWithFilter(this.t,
        FuncExp((x: Exp[T]) => IsDefinedAt(PartialFuncExp(f), x))),
        FuncExp((x: Exp[T]) => App(PartialFuncExp(f), x)))(c)
    }

    def filter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
      newFilter(this.t, FuncExp(f))

    def union[U >: T, That <: Traversable[U]](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newUnion(this.t, that)

    // XXX: This cannot be called + to avoid ambiguity with the conversion to NumericOps - probably that's an artifact of it being
    // declared in a subclass
    def :+[U >: T, That <: Traversable[U]](that: Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      this union onExp(that)('Traversable, Traversable(_))
    def ++[U >: T, That <: Traversable[U]](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      union(that)

    def size = Size(this.t)
    def length = size

    //XXX: Generate these wrappers
    def toSet = onExp(this.t)('toSet, _.toSet)
    def isEmpty: Exp[Boolean] = IsEmpty(this.t)
    def nonEmpty: Exp[Boolean] = !isEmpty

    def view: Exp[TraversableView[T, Repr]] = View(this.t)

    def groupBy[K](f: Exp[T] => Exp[K]): Exp[Map[K, Repr]] =
      GroupBy(this.t, FuncExp(f))

    def groupBySel[K, Rest, That <: Traversable[Rest]](f: Exp[T] => Exp[K], g: Exp[T] => Exp[Rest])(implicit c: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]] =
      groupBySelImpl(this.t, f, g)(c)

    def join[S, TKey, TResult, That](innerColl: Exp[Traversable[S]]) //Split argument list to help type inference deduce S and use it after.
                                    (outerKeySelector: Exp[T] => Exp[TKey],
                                     innerKeySelector: Exp[S] => Exp[TKey],
                                     resultSelector: Exp[(T, S)] => Exp[TResult])
                                    (implicit cbf: CanBuildFrom[Repr, TResult, That]): Exp[That]
    = Join(this.t, innerColl, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))

    def forall(f: Exp[T] => Exp[Boolean]) = Forall(this.t, FuncExp(f))
    //This awkward form is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    def exists(f: Exp[T] => Exp[Boolean]) = !(Forall(this.t, FuncExp(f andThen (!(_)))))

    def typeFilter[S](implicit cS: ClassManifest[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      TypeFilter[T, Traversable, ID, S](t, FuncExp(identity))
    }
  }

  trait TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    Coll[X] <: Traversable[X] with TraversableLike[X, Coll[X]],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl] with Coll[T]]
    extends TraversableLikeOps[T, Coll, ViewColl] with WithFilterable[T, Repr]
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]) = Force(this.t)

    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[ViewColl] =
      newFilter[T, ViewColl](this.t, FuncExp(f))
    //TODO: override operations to avoid using CanBuildFrom
  }

  class TraversableOps[T](val t: Exp[Traversable[T]]) extends TraversableLikeOps[T, Traversable, Traversable[T]] with WithFilterImpl[T, Traversable[T], Traversable[T]]

  class TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](val t: Exp[TraversableView[T, Repr]])
    extends TraversableViewLikeOps[T, Repr, Traversable, TraversableView[T, Repr]]

  implicit def expToTravExp[T](t: Exp[Traversable[T]]): TraversableOps[T] = new TraversableOps(t)
  implicit def tToTravExp[T](t: Traversable[T]): TraversableOps[T] = {
    //toExp(t)
    expToTravExp(t)
  }

  implicit def expToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[TraversableView[T, Repr]]): TraversableViewOps[T, Repr] = new TraversableViewOps(t)
  implicit def tToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: TraversableView[T, Repr]): TraversableViewOps[T, Repr] = expToTravViewExp(t)

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Exp[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]] = expToTravViewExp(
    t.asInstanceOf[Exp[TraversableView[T, C[T]]]])
  //XXX
  implicit def tToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: TraversableView[T, C[_]]): TraversableViewOps[T, C[T]] = expToTravViewExp2(t)
}

trait ForceOps {
  this: LiftingConvs with TraversableOps =>

  sealed trait Forceable[T, Coll] {
    def force(t: Coll): Traversable[T]
    def force(t: Exp[Coll]): Exp[Traversable[T]]
  }
  implicit def TraversableForceable[T]: Forceable[T, Traversable[T]] = new Forceable[T, Traversable[T]] {
    def force(t: Traversable[T]) = t
    def force(t: Exp[Traversable[T]]) = t
  }
  //Note: type inference does not pick supertypes of arguments unless needed (i.e. if inferring T from t: T, the type
  //of T will be picked usually), therefore this implicit will be picked when needed. Note that since Forceable is invariant,
  //implicit resolution will not have other alternatives
  implicit def TraversableViewForceable[T]: Forceable[T, TraversableView[T, Traversable[_]]] = new Forceable[T, TraversableView[T, Traversable[_]]] {
    def force(t: Exp[TraversableView[T, Traversable[_]]]) = t.force
    def force(t: TraversableView[T, Traversable[_]]) = t.force
  }

  //Note: "Coll with Traversable[T]" seems to help deduction of T, since apparently the type-class parameter is not enough
  // for that.
  implicit def pimpForce[T, Coll](t: Coll with Traversable[T])(implicit f: Forceable[T, Coll]) = new ForceOps[T, Coll](t)
  class ForceOps[T, Coll](t: Coll)(implicit f: Forceable[T, Coll]) {
    def force: Traversable[T] = f.force(t)
  }

  implicit def pimpForceExp[T, Coll](t: Exp[Coll])(implicit f: Forceable[T, Coll]) = new ForceOpsExp(t)
  class ForceOpsExp[T, Coll](t: Exp[Coll])(implicit f: Forceable[T, Coll]) {
    def force: Exp[Traversable[T]] = f.force(t)
  }
}
/**
 * A goal of this new encoding is to be able to build expression trees (in particular, query trees) producing
 * different collections; once we can represent query trees producing maps and maintain them incrementally, view
 * maintenance can subsume index update.
 */

//XXX: we'll probably have to duplicate this for Maps, as for Sets below. Or rather, we could drop this if we define
//implicit conversions for both WithFilterImpl and TraversableLikeOps.
trait MapOps {
  this: LiftingConvs with TraversableOps =>
  class MapOps[K, V](val t: Exp[Map[K, V]]) extends TraversableLikeOps[(K, V), Iterable, Map[K, V]] with WithFilterImpl[(K, V), Map[K, V], Map[K, V]] {
    /*
    //IterableView[(K, V), Map[K, V]] is not a subclass of Map; therefore we cannot simply return Exp[Map[K, V]].
    case class WithFilter(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[IterableView[(K, V), Map[K, V]]] {
      override def interpret = base.interpret.view filter f.interpret
    }
    */
  }

  implicit def expToMapExp[K, V](t: Exp[Map[K, V]]): MapOps[K, V] = new MapOps(t)
  implicit def tToMapExp[K, V](t: Map[K, V]): MapOps[K, V] =
    expToMapExp(t)
}

trait CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  //We want this lifting to apply to all Sets, not just the immutable ones, so that we can call map also on IncHashSet
  //and get the right type.
  import collection.{Set => GenSet}

  case class Contains[T](set: Exp[GenSet[T]], v: Exp[T]) extends BinaryOpExp[GenSet[T], T, Boolean, Contains[T]](set, v) {
    def interpret() = set.interpret().contains(v.interpret())
    def copy(set: Exp[GenSet[T]], v: Exp[T]) = Contains(set: Exp[GenSet[T]], v: Exp[T])
  }

  class CollectionGenSetOps[T](val t: Exp[GenSet[T]]) extends TraversableLikeOps[T, GenSet, GenSet[T]] with WithFilterImpl[T, GenSet[T], GenSet[T]] {
    def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
    def contains(el: Exp[T]) = apply(el)
    def --(that: Exp[GenTraversableOnce[T]]): Exp[GenSet[T] /* Repr */] =
      Diff(t, that)
  }
  implicit def expToCollectionGenSetExp[T](t: Exp[GenSet[T]]): CollectionGenSetOps[T] = new CollectionGenSetOps(t)
  implicit def tToCollectionGenSetExp[T](t: GenSet[T]): CollectionGenSetOps[T] = expToCollectionGenSetExp(t)
}

trait SetOps extends CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  //For convenience, also have a lifting for scala.Set = scala.collection.immutable.Set.

  // This class differs from CollectionSetOps because it extends TraversableLikeOps[T, collection.immutable.Set, collection.immutable.Set[T]]
  // instead of TraversableLikeOps[T, collection.Set, collection.Set[T]].
  // XXX: abstract the commonalities in SetLikeOps, even if for now it takes more code than it saves.
  class SetOps[T](val t: Exp[Set[T]]) extends TraversableLikeOps[T, Set, Set[T]] with WithFilterImpl[T, Set[T], Set[T]] {
    def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
    def contains(el: Exp[T]) = apply(el)
  }
  implicit def expToSetExp[T](t: Exp[Set[T]]): SetOps[T] = new SetOps(t)
  implicit def tToSetExp[T](t: Set[T]): SetOps[T] = expToSetExp(t)
}

trait TypeFilterOps {
  this: TupleOps with FunctionOps with TraversableOps =>
  case class GroupByType[T, C[X] <: TraversableLike[X, C[X]], D[_]](base: Exp[C[D[T]]], f: Exp[D[T] => T]) extends BinaryOpExp[C[D[T]], D[T] => T, TypeMapping[C, D],
    GroupByType[T, C, D]](base, f) {
    override def interpret() = {
      val x: C[D[T]] = base.interpret()
      val g: D[T] => T = f.interpret()

      //Why the null check? Remember that (null instanceof Foo) = false. Hence, without using the index, "if (a instanceof Foo)" subsumes
      //a != null. Here we need to do that check otherwise.
      new TypeMapping[C, D](x.filter(g(_) != null).groupBy
        ((x: D[T] /* T */) => ClassManifest.fromClass(g(x).getClass)).asInstanceOf[Map[ClassManifest[_], C[D[_]]]])
    }
    override def copy(base: Exp[C[D[T]]], f: Exp[D[T]=>T]) = GroupByType[T, C, D](base, f)
  }

  /*
   * failed attempt to code GroupByType without type cast
    case class GroupByType[T, C[X] <: Traversable[X], Repr <: TraversableLike[T, Repr]](base: Exp[C[T] with Repr]) extends UnaryOpExp[C[T] with Repr, TypeMapping[C]](base) {
    override def interpret() = {
      val x: C[T] with Repr = base.interpret()
      new TypeMapping[C](x.groupBy[ClassManifest[_]]( (_: Any) => ClassManifest.Int).asInstanceOf[Map[ClassManifest[_], C[_]]])
      //x.groupBy[ClassManifest[_]]( (x:C[T])  => ClassManifest.fromClass(x.getClass).asInstanceOf[ClassManifest[_]])
    }
    override def copy(base: Exp[C[T] with Repr]) = GroupByType[T, C, Repr](base)
  }
 */
  case class TypeMappingApp[C[X] <: TraversableLike[X, C[X]], D[_], S](base: Exp[TypeMapping[C, D]])(implicit cS: ClassManifest[S])
    extends UnaryOpExp[TypeMapping[C, D], C[D[S]], TypeMappingApp[C, D, S]](base) {
    override def copy(base: Exp[TypeMapping[C, D]]) = TypeMappingApp[C, D, S](base)
    override def interpret() = {
      base.interpret().get[S]
    }

  }
  class TypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](val t: Exp[C[D[T]]]) {
    def typeFilterWith[S](f: Exp[D[T]] => Exp[T])(implicit cS: ClassManifest[S]) = TypeFilter[T, C, D, S](t, FuncExp(f))
    def groupByType(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t, FuncExp(f))
  }

  class TypeMappingAppOps[C[X] <: TraversableLike[X, C[X]], D[_]](val t: Exp[TypeMapping[C, D]]) {
    def get[S](implicit cS: ClassManifest[S]) = TypeMappingApp[C, D, S](t)
  }
  implicit def expToTypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](t: Exp[C[D[T]]]) = new TypeFilterOps[T, C, D](t)
  implicit def expToTypeMappingAppOps[C[X] <: TraversableLike[X, C[X]], D[+_]](t: Exp[TypeMapping[C, D]]) = new TypeMappingAppOps[C, D](t)
  //Experiments
  class GroupByTupleType[U, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](val t: Exp[C[U]]) {
    def groupByTupleType[T, D[_]](typeEqual: U =:= D[T])(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t map (x => onExp(x)('foo, typeEqual)), FuncExp(f))
  }
  //XXX: Copied from Scalaz for testing - this should be _temporary_!
  trait PartialApply1Of2[T[_, _], A] {
    type Apply[B] = T[A, B]

    type Flip[B] = T[B, A]
  }

  class GroupByTupleTypeOps[T, U, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType1 /*(f: Exp[(T, U)] => Exp[T]) */ = GroupByType[T, C, PartialApply1Of2[Tuple2, U]#Flip](this.t, FuncExp(_._1))
    def groupByTupleType2 /*(f: Exp[(T, U)] => Exp[U]) */ = GroupByType[U, C, PartialApply1Of2[Tuple2, T]#Apply](this.t, FuncExp(_._2))
  }
  implicit def expToGroupByTupleType[T, U, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps(t)
}
