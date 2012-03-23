package ivm.expressiontree

import collection.generic.CanBuildFrom
import collection.{GenTraversableView, TraversableView, TraversableViewLike, TraversableLike}
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
  def newFlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr], f: FuncExp[T, Traversable[U]])
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
    def flatMap[U, That <: Traversable[U]](f: Exp[T] => Exp[Traversable[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newFlatMap(this.t, FuncExp(f))
  }

  //This is just an interface for documentation purposes.
  trait WithFilterable[T, Repr] {
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]]
    def exists(f: Exp[T] => Exp[Boolean]) = !IsEmpty(this withFilter f)//(withFilter f).isEmpty
    //The awkward use of andThen below is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    def forall(f: Exp[T] => Exp[Boolean]) = IsEmpty(this withFilter (f andThen (!(_)))) //Forall(this.t, FuncExp(f))
  }

  trait WithFilterImpl[T, Repr <: Traversable[T] with TraversableLike[T, Repr]] extends WithFilterable[T, Repr] {
    this: FilterMonadicOpsLike[T, Repr] =>
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[TraversableView[T, Repr]] =
      newWithFilter(this.t, FuncExp(f))
  }

  def groupBySelImpl[T, Repr <: Traversable[T] with
    TraversableLike[T, Repr], K, Rest, That <: Traversable[Rest]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                                                  g: Exp[T] => Exp[Rest])(
    implicit c: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]]

  //Coll is only needed for TypeFilter.
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
      this union Traversable(that)
    def ++[U >: T, That <: Traversable[U]](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      union(that)

    def size = Size(this.t)
    def length = size
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

    //def forall(f: Exp[T] => Exp[Boolean]) = Forall(this.t, FuncExp(f))
    //This awkward form is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    //def exists(f: Exp[T] => Exp[Boolean]) = !(Forall(this.t, FuncExp(f andThen (!(_)))))

    def typeFilter[S](implicit cS: ClassManifest[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, FuncExp(identity[Exp[T]]), cS) //variance mismatch
      TypeFilter[T, Traversable, ID, S](t, FuncExp(identity[Exp[T]]), cS)
    }
    private[ivm] def typeFilterClass[S](classS: Class[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, FuncExp(identity[Exp[T]]), classS) //variance mismatch again
      TypeFilter[T, Traversable, ID, S](t, FuncExp(identity[Exp[T]]), classS)
    }

    //XXX: Generate these wrappers, also for other methods.
    def toSet = onExp(this.t)('TraversableLike$toSet, _.toSet)
    def toSeq = onExp(this.t)('TraversableLike$toSeq, _.toSeq)

    def typeCase[Res, That <: TraversableLike[Res, That]](cases: TypeCase[_, Res]*)(implicit c: CanBuildFrom[TraversableView[T, Repr], Res, That]): Exp[That] = TypeCaseExp(this.t, cases)
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

  class TraversableOps[T](val t: Exp[Traversable[T]]) extends TraversableLikeOps[T, Traversable, Traversable[T]] with WithFilterImpl[T,  Traversable[T]]

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

  implicit def TraversableExp2ExpTraversable[T](e: Traversable[Exp[T]]): Exp[Traversable[T]] = ExpSeq(e.toList) //onExp(e)('Traversable, Traversable(_))
  implicit def SetExp2ExpSet[T](e: Set[Exp[T]]): Exp[Set[T]] = ExpSeq(e.toList).toSet
}

trait ForceOps {
  this: LiftingConvs with TraversableOps =>

  sealed trait Forceable[T, Coll] {
    def force(t: Coll): Traversable[T]
    def force(t: Exp[Coll]): Exp[Traversable[T]]
  }
  implicit def TraversableForceable[T]: Forceable[T, Traversable[T]] = new Forceable[T, Traversable[T]] {
    def force(t: Traversable[T]) = t match {
      //Note: this class name is the root of all views for Scala 2.9, but this is not part of the API.
      case view: GenTraversableView[_, _] => view.asInstanceOf[GenTraversableView[T, Traversable[T]]].force
      case coll => coll
    }
    def force(t: Exp[Traversable[T]]) = ForceIfPossible(t)
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
    //A bit of a hack, since it does not return the most precise type possible.
    def force: Traversable[T] = f.force(t)
  }

  implicit def pimpForceExp[T, Coll](t: Exp[Coll])(implicit f: Forceable[T, Coll]) = new ForceOpsExp(t)
  class ForceOpsExp[T, Coll](t: Exp[Coll])(implicit f: Forceable[T, Coll]) {
    //A bit of a hack, since it does not return the most precise type possible.
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
trait CollectionMapOps {
  this: LiftingConvs with TraversableOps with FunctionOps =>
  import collection.{Map, MapLike}

  trait MapLikeOps[K, V, Coll[K, V] <: Map[K, V] with MapLike[K, V, Coll[K, V]]]
    extends TraversableLikeOps[(K, V), Iterable, Coll[K, V]] with WithFilterImpl[(K, V), Coll[K, V]] {
    def get(key: Exp[K]): Exp[Option[V]] = onExp(t, key)('Map$get, _ get _)
    /*
    //IterableView[(K, V), Map[K, V]] is not a subclass of Map; therefore we cannot simply return Exp[Map[K, V]].
    case class WithFilter(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[IterableView[(K, V), Map[K, V]]] {
      override def interpret = base.interpret.view filter f.interpret
    }
    */
  }

  class CollectionMapOps[K, V](val t: Exp[Map[K, V]]) extends MapLikeOps[K, V, Map]

  implicit def expToCollectionMapExp[K, V](t: Exp[Map[K, V]]): CollectionMapOps[K, V] = new CollectionMapOps(t)
  implicit def tToCollectionMapExp[K, V](t: Map[K, V]): CollectionMapOps[K, V] =
    expToCollectionMapExp(t)
}

trait MapOps extends CollectionMapOps {
  this: LiftingConvs with TraversableOps with FunctionOps =>

  class MapOps[K, V](val t: Exp[Map[K, V]]) extends MapLikeOps[K, V, Map]

  implicit def expToMapExp[K, V](t: Exp[Map[K, V]]): MapOps[K, V] = new MapOps(t)
  implicit def tToMapExp[K, V](t: Map[K, V]): MapOps[K, V] =
    expToMapExp(t)
}

trait IterableOps {
  this: LiftingConvs with TraversableOps =>
  class IterableOps[T](val t: Exp[Iterable[T]]) extends TraversableLikeOps[T, Iterable, Iterable[T]] with WithFilterImpl[T, Iterable[T]]

  implicit def expToIterableExp[T](t: Exp[Iterable[T]]): IterableOps[T] = new IterableOps(t)
  implicit def tToIterableExp[T](t: Iterable[T]): IterableOps[T] =
    expToIterableExp(t)
}

//Unlike for other collection, Seq by default refers to collection.Seq, not to collection.immutable.Seq
trait CollectionSeqOps {
  this: LiftingConvs with TraversableOps =>
  import collection.SeqLike
  trait SeqLikeOps[T, Coll[+T] <: Seq[T] with SeqLike[T, Coll[T]]] extends TraversableLikeOps[T, Coll, Coll[T]] with WithFilterImpl[T, Coll[T]]

  class CollectionSeqOps[T](val t: Exp[Seq[T]]) extends SeqLikeOps[T, Seq]

  implicit def CollectionSeqExp2ExpSeq[T](e: Seq[Exp[T]]): Exp[Seq[T]] = ExpSeq(e.toList)

  implicit def expToCollectionSeqExp[T](t: Exp[Seq[T]]): CollectionSeqOps[T] = new CollectionSeqOps(t)
  implicit def tToCollectionSeqExp[T](t: Seq[T]): CollectionSeqOps[T] =
    expToCollectionSeqExp(t)
}

trait SeqOps extends CollectionSeqOps {
  this: LiftingConvs with TraversableOps =>
  import collection.immutable.Seq

  class SeqOps[T](val t: Exp[Seq[T]]) extends SeqLikeOps[T, Seq]

  implicit def SeqExp2ExpSeq[T](e: Seq[Exp[T]]): Exp[Seq[T]] = ExpSeq(e)

  implicit def expToSeqExp[T](t: Exp[Seq[T]]): SeqOps[T] = new SeqOps(t)
  implicit def tToSeqExp[T](t: Seq[T]): SeqOps[T] =
    expToSeqExp(t)
}

trait CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  //We want this lifting to apply to all Sets, not just the immutable ones, so that we can call map also on IncHashSet
  //and get the right type.
  import collection.{SetLike, Set}

  trait SetLikeOps[T, Coll[T] <: Set[T] with SetLike[T, Coll[T]]]
    extends TraversableLikeOps[T, Coll, Coll[T]] with WithFilterImpl[T, Coll[T]] {
    def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
    def contains(el: Exp[T]) = apply(el)
    def --(that: Exp[Traversable[T]]): Exp[Coll[T]] =
      Diff(t, that)
  }
  class CollectionSetOps[T](val t: Exp[Set[T]]) extends SetLikeOps[T, Set]
  implicit def expToCollectionSetExp[T](t: Exp[Set[T]]): CollectionSetOps[T] = new CollectionSetOps(t)
  implicit def tToCollectionSetExp[T](t: Set[T]): CollectionSetOps[T] = expToCollectionSetExp(t)
}

trait SetOps extends CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  //For convenience, also have a lifting for scala.Set = scala.collection.immutable.Set.

  // This class differs from CollectionSetOps because it extends TraversableLikeOps[T, collection.immutable.Set, collection.immutable.Set[T]]
  // instead of TraversableLikeOps[T, collection.Set, collection.Set[T]].
  class SetOps[T](val t: Exp[Set[T]]) extends SetLikeOps[T, Set]
  implicit def expToSetExp[T](t: Exp[Set[T]]): SetOps[T] = new SetOps(t)
  implicit def tToSetExp[T](t: Set[T]): SetOps[T] = expToSetExp(t)
}

sealed trait MaybeSub[-A, +B]
case class YesSub[-A, +B](implicit val p: A <:< B) extends MaybeSub[A, B]
case object NoSub extends MaybeSub[Any, Nothing]

trait LowPriority {
    implicit def noSub = NoSub
}
object MaybeSub extends LowPriority {
    implicit def yesSub[A, B](implicit p: A <:< B) = YesSub[A, B]
}

import collection.{immutable, TraversableLike, mutable}
import collection.generic.CanBuildFrom
import mutable.{Queue, ArrayBuffer, Builder}

object CollectionUtils {
  //This is equivalent to coll.collectFirst(Function.unlift(f)), but it saves the expensive Function.unlift.
  def collectFirst[T, U](coll: Traversable[T])(f: T => Option[U]): Option[U] = {
    for (x <- coll) {
      f(x) match {
        case v@Some(_) => return v
        case _ =>
      }
    }
    None
  }

  //This must be only used inside the implementation. Mutability fun!
  /*private*/ def groupBySel[A, K, B, Repr <: Traversable[A], That](coll: Repr with Traversable[A])(f: A => K, g: A => B)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[B, That]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, cbf(coll))
      bldr += g(elem)
    }
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result()
  }

  /*private*/ def groupBySelAndForeach[A, K, B, Repr <: TraversableLike[A, Repr], That](coll: Repr with TraversableLike[A, Repr])(f: A => K, g: A => B)(h: K => Unit)(implicit cbf: CanBuildFrom[Repr, B, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[B, That]]
    for (elem <- coll) {
      val key = f(elem)
      if (key != null) { //Special
        val bldr = m.getOrElseUpdate(key, cbf(coll))
        bldr += g(elem)
        h(key) //Special
      }
    }
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result
  }
}

object TypeHierarchyUtils {
  import CollectionUtils._

  //returns all b such that a R* b, where R is the relation represented by map.
  /*private*/ def transitiveQuery[T](map: Map[T, Set[T]], a: T): Set[T] = {
    for {
      b <- map.get(a).getOrElse(Set())
      c <- transitiveQuery(map, b) + b
    } yield c
  }

  //Class.getSuperclass can return null, filter that out. Now make sure that Object is always included? Add testcases for primitive types?
  /*private*/ def superClass(c: Class[_]): Option[Class[_]] = Option(c.getSuperclass) orElse (if (c == classOf[Any]) None else Some(classOf[Any]))
  /*private*/ def superInterfaces(c: Class[_]): Seq[Class[_]] = c.getInterfaces
  //getInterfaces returns all implemented interfaces :-), while getSuperclass does not.
  /*private*/ def superTypes(c: Class[_]): Seq[Class[_]] = superInterfaces(c) ++ superClass(c)


  //TODO: have a per-thread global map, so that the reconstructed type hierarchy can be shared between different indexes.
  //Problem there: GC of entries.
  //Contract: Returns a map defined on interfaces and classes, which returns their implementing classes and possibly
  //their implementing interfaces.
  //With this contract, given a type, we can find its concrete subtypes, and look them up in a type index.
  //XXX Careful: T must be passed explicitly! Otherwise it will be deduced to be Nothing
  def computeSubTypeRel[T: ClassManifest](seenTypes: Set[Class[_]]): immutable.Map[Class[_], Set[Class[_]]] = {
    //val subtypeRel = mutable.Set.empty[(Class[_], Class[_])]
    val subtypeRel = ArrayBuffer.empty[(Class[_], Class[_])]
    val classesToScan: Queue[Class[_]] = Queue()
    def add(clazz: Class[_]) {
      val superTypesClazz = superTypes(clazz)
      classesToScan enqueue (superTypesClazz: _*)
      for (superType <- superTypesClazz)
        subtypeRel += (superType -> clazz) //Map s to its subtypes.
    }
    val erasedT = ClassUtil.boxedErasure(classManifest[T])
    for {
      clazz <- seenTypes
      if clazz != erasedT
      s <- superTypes(clazz)
    } {
      add(clazz)
    }
    //For each supertype found, look up its superclasses.
    while (classesToScan.nonEmpty) {
      add(classesToScan.dequeue())
    }
    // Since we look up only concrete types, we needn't represent the subtype relationships between interfaces.
    //However, this optimization is hardly significant since this code takes a quite small amount of time, compared to
    //iterating over the values themselves.
    groupBySel(subtypeRel)(_._1, _._2)(collection.breakOut)
  }
}

trait TypeFilterOps {
  this: LiftingConvs with TupleOps with FunctionOps with TraversableOps =>
  case class GroupByType[T, C[X] <: TraversableLike[X, C[X]], D[+_]](base: Exp[C[D[T]]], f: Exp[D[T] => T])(implicit cbf: CanBuildFrom[C[D[T]], D[T], C[D[T]]], cm: ClassManifest[T]) extends Arity2OpExp[C[D[T]], D[T] => T, TypeMapping[C, D, T],
    GroupByType[T, C, D]](base, f) {
    import CollectionUtils._
    import TypeHierarchyUtils._

    override def interpret() = {
      val coll: C[D[T]] = base.interpret()
      val g: D[T] => T = f.interpret()
      val seenTypes = Set.newBuilder[Class[_]]
      def getType(x: D[T]): Class[_] = {
        val gx = g(x)
        //Why the null check? Remember that (null instanceof Foo) = false. Hence, without using the index, "if (a instanceof Foo)" subsumes
        //a != null. Here we need to do that check otherwise. To avoid a separate filter stage, and since views don't really support groupBy,
        //aggregate nulls into a separate class.
        if (gx != null)
          ClassUtil.primitiveToBoxed(gx.getClass)
        else
          null
      }

      //val map = coll groupBy getType
      val map: Map[Class[_], C[D[T]]] = groupBySelAndForeach(coll)(getType, identity)(seenTypes += _)(cbf)
      val subtypeRel = computeSubTypeRel[T](seenTypes.result())

      new TypeMapping[C, D, T](map, subtypeRel, coll)
    }
    override def copy(base: Exp[C[D[T]]], f: Exp[D[T] => T]) = GroupByType[T, C, D](base, f)
  }

  /*
   * failed attempt to code GroupByType without type cast
    case class GroupByType[T, C[X] <: Traversable[X], Repr <: TraversableLike[T, Repr]](base: Exp[C[T] with Repr]) extends Arity1OpExp[C[T] with Repr, TypeMapping[C]](base) {
    override def interpret() = {
      val x: C[T] with Repr = base.interpret()
      new TypeMapping[C](x.groupBy[ClassManifest[_]]( (_: Any) => ClassManifest.Int).asInstanceOf[Map[ClassManifest[_], C[_]]])
      //x.groupBy[ClassManifest[_]]( (x:C[T])  => ClassManifest.fromClass(x.getClass).asInstanceOf[ClassManifest[_]])
    }
    override def copy(base: Exp[C[T] with Repr]) = GroupByType[T, C, Repr](base)
  }
 */
  case class TypeMappingApp[C[+X] <: TraversableLike[X, C[X]], D[+_], Base, T](base: Exp[TypeMapping[C, D, Base]], classS: Class[_])
                                                                              (implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]])
    extends Arity1OpExp[TypeMapping[C, D, Base], C[D[T]], TypeMappingApp[C, D, Base, T]](base) {
    override def copy(base: Exp[TypeMapping[C, D, Base]]) = TypeMappingApp[C, D, Base, T](base, classS)
    override def interpret() =
      base.interpret().get[T](classS)
  }

  class TypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](val t: Exp[C[D[T]]]) {
    def typeFilterWith[S](f: Exp[D[T]] => Exp[T])(implicit cS: ClassManifest[S]) = TypeFilter[T, C, D, S](t, FuncExp(f), cS)
    //def groupByType(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t, FuncExp(f))
  }
  implicit def expToTypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](t: Exp[C[D[T]]]) = new TypeFilterOps[T, C, D](t)

  class TypeMappingAppOps[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](val t: Exp[TypeMapping[C, D, Base]]) {
    def get[T](implicit cS: ClassManifest[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]) = get[T](ClassUtil.boxedErasure(cS))
    def get[T](classS: Class[_])(implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]) = TypeMappingApp[C, D, Base, T](t, classS)
  }
  implicit def expToTypeMappingAppOps[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](t: Exp[TypeMapping[C, D, Base]]) = new TypeMappingAppOps[C, D, Base](t)

  //Experiments
  /*
  class GroupByTupleType[U, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](val t: Exp[C[U]]) {
    def groupByTupleType[T, D[_]](typeEqual: U =:= D[T])(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t map (x => onExp(x)('foo, typeEqual)), FuncExp(f))
  }
  */
  //Copied from Scalaz and altered a bit. Not sure all this generality is useful since we only use it for tuples.
  trait PartialApply1Of2[T[+_, +_], A] {
    type Apply[+B] = T[A, B]
    type Flip[+B] = T[B, A]
  }

  class GroupByTupleTypeOps1[T: ClassManifest, U, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType1(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[T]) */ = GroupByType[T, C, PartialApply1Of2[Tuple2, U]#Flip](this.t, FuncExp(_._1))
  }
  implicit def expToGroupByTupleType1[T: ClassManifest, U, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps1(t)

  class GroupByTupleTypeOps2[T, U: ClassManifest, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType2(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[U]) */ = GroupByType[U, C, PartialApply1Of2[Tuple2, T]#Apply](this.t, FuncExp(_._2))
  }
  implicit def expToGroupByTupleType2[T, U: ClassManifest, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps2(t)

  //typeCase method

  //def when[Case, Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassManifest[Case]) = TypeCase(cS, FuncExp(f))
  trait WhenResult[Case] {
    private[ivm] def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]): TypeCase[Case, Res]
    def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassManifest[Case]): TypeCase[Case, Res]
    def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: ClassManifest[Case]): TypeCase[Case, Res]
  }

  object when {
    def apply[Case] = new WhenResult[Case] {
      private[ivm] override def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]) =
        TypeCase(classS,
          FuncExp(guard),
          FuncExp(f))
      override def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: ClassManifest[Case]) =
        onClass[Res](guard, f, ClassUtil.boxedErasure(cS))
      override def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassManifest[Case]) =
        apply(_ => true, f)
    }
  }
}
