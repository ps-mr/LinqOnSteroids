package ivm.expressiontree

import collection.generic.CanBuildFrom
import collection.{GenTraversableView, TraversableView, TraversableViewLike, TraversableLike}
import ivm.collections.TypeMapping

trait TraversableOps {
  this: BaseExps with BaseTypesOps =>
  def newFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr],
                                                                             f: Fun[T, Boolean]) =
    Filter(base, f)
  def newWithFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr],
                                                                             f: Fun[T, Boolean]) =
    newFilter(base, f)
  def newMapOp[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U] with TraversableLike[U, That]](base: Exp[Repr],
                                                                                                   f: Fun[T, U])
                                                                                                  (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    MapNode(base, f)
  def newFlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr], f: Fun[T, Traversable[U]])
                                        (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    FlatMap(base, f)

  def newUnion[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U >: T, That <: Traversable[U]](base: Exp[Repr with Traversable[T]], that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    Union(base, that)

  def groupBySelImpl[T, Repr <: Traversable[T] with
    TraversableLike[T, Repr], K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                                                                                   g: Exp[T] => Exp[Rest])(
    implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]]

  trait Holder[Repr] {
    val t: Exp[Repr]
  }

  class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](protected val t: Exp[Repr]) {
    def map[U, That <: Traversable[U] with TraversableLike[U, That]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newMapOp(this.t, Fun(f))
    def flatMap[U, That <: Traversable[U]](f: Exp[T] => Exp[Traversable[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newFlatMap(this.t, Fun(f))
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
      newWithFilter(this.t, Fun(f))

    def exists(f: Exp[T] => Exp[Boolean]) = !IsEmpty(this withFilter f)//(withFilter f).isEmpty
    //The awkward use of andThen below is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    def forall(f: Exp[T] => Exp[Boolean]) = IsEmpty(this withFilter (f andThen (!(_)))) //Forall(this.t, Fun(f))

    def collect[U, That <: Traversable[U] with TraversableLike[U, That]](f: Exp[T] => Exp[Option[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] = {
      newMapOp(newWithFilter(this.t,
        Fun((x: Exp[T]) => IsDefinedAt(PartialFuncExp(f), x))),
        Fun((x: Exp[T]) => App(PartialFuncExp(f), x)))(c)
    }

    def filter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
      newFilter(this.t, Fun(f))

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

    def indexBy[K](f: Exp[T] => Exp[K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, Repr]): Exp[Map[K, Repr]] =
      IndexBy(this.t, Fun(f))

    def groupBySel[K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](f: Exp[T] => Exp[K], g: Exp[T] => Exp[Rest])(implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]] =
      groupBySelImpl(this.t, f, g)(cbf, cbf2)

    def join[S, TKey, TResult, That](innerColl: Exp[Traversable[S]]) //Split argument list to help type inference deduce S and use it after.
                                    (outerKeySelector: Exp[T] => Exp[TKey],
                                     innerKeySelector: Exp[S] => Exp[TKey],
                                     resultSelector: Exp[(T, S)] => Exp[TResult])
                                    (implicit cbf: CanBuildFrom[Repr, TResult, That]): Exp[That]
    = Join(this.t, innerColl, Fun(outerKeySelector), Fun(innerKeySelector), Fun(resultSelector))

    //def forall(f: Exp[T] => Exp[Boolean]) = Forall(this.t, Fun(f))
    //This awkward form is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    //def exists(f: Exp[T] => Exp[Boolean]) = !(Forall(this.t, Fun(f andThen (!(_)))))

    def typeFilter[S](implicit cS: reflect.ClassTag[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, Fun(identity[Exp[T]]), cS) //variance mismatch
      TypeFilter[T, Traversable, ID, S](t, Fun(identity[Exp[T]]), cS, classManifest[Traversable[S]].toString)
    }
    private[ivm] def typeFilterClass[S](classS: Class[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, Fun(identity[Exp[T]]), classS) //variance mismatch again
      TypeFilter[T, Traversable, ID, S](t, Fun(identity[Exp[T]]), classS, "Traversable[%s]" format classS.getName)
    }

    //XXX: Generate these wrappers, also for other methods.
    def toSet = fmap(this.t, 'TraversableLike)('toSet, _.toSet)
    def toSeq = fmap(this.t, 'TraversableLike)('toSeq, _.toSeq)
    def flatten[U](implicit asTraversable: T => TraversableOnce[U]) = fmap(this.t, 'TraversableLikeOps)('flatten, _.flatten)

    def typeCase[Res](cases: TypeCase[_, Res]*): Exp[Set[Res]] = TypeCaseExp(this.t, cases)
  }

  class TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]](tp: Exp[ViewColl])
    extends TraversableLikeOps[T, ViewColl](tp)
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]) = Force(this.t)

    //def withFilter(f: Exp[T] => Exp[Boolean]): Exp[ViewColl] =
      //newFilter[T, ViewColl](this.t, Fun(f))
    //TODO: override operations to avoid using CanBuildFrom
  }

  //Compare collection nodes by identity.
  //Saves costs when comparing collections, which happens during optimization.
  implicit def pureColl[T, Repr <: Traversable[T] with TraversableLike[T, Repr]: reflect.ClassTag](v: Repr with Traversable[T]): Exp[Repr] =
    ConstByIdentity(v)

  //This version does not work, due to https://issues.scala-lang.org/browse/SI-5298:
  //implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Exp[Repr])
  /*
  //With this version, we need an extra overload for Range <: IndexedSeq[Int] <: IndexedSeqLike[Int, IndexedSeq[Int]].
  implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Exp[Repr with Traversable[T]]) =
    new TraversableLikeOps[T, Repr](v)
  implicit def toTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Repr with Traversable[T]) =
    expToTraversableLikeOps(v)
  implicit def expRangeToTraversableLikeOps(r: Exp[Range]) = expToTraversableLikeOps(r: Exp[IndexedSeq[Int]])
  */
  /* Instead, use TraversableLike[T, Repr] in the bound, so that it can be considered when looking for a matching Repr;
   * Repr is now deduced by matching against TraversableLike[T, Repr]. At least `with` is still commutative - this works
   * irrespective of the order in which the bounds are considered. */
  implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Exp[Repr with TraversableLike[T, Repr]]) =
    new TraversableLikeOps[T, Repr](v)
  implicit def toTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]: reflect.ClassTag](v: Repr with TraversableLike[T, Repr]) =
    expToTraversableLikeOps(v)

  //Ranges are cheap to compare for equality; hence we can easily use pure, not pureColl, for them.
  implicit def pureRange(r: Range): Exp[Range] = pure(r)
  //Repr = Range does not satisfy the bound given by:
  //  Repr <: Traversable[T] with TraversableLike[T, Repr]
  //Hence we need this extra conversion.
  implicit def rangeToTraversableLikeOps(r: Range) = expToTraversableLikeOps(r: Exp[IndexedSeq[Int]])

  //XXX: this class is now essentially useless.
  class TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](tp: Exp[TraversableView[T, Repr]])
    extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]](tp)

  implicit def expToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[TraversableView[T, Repr]]): TraversableViewOps[T, Repr] = new TraversableViewOps(t)
  implicit def tToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]: reflect.ClassTag](t: TraversableView[T, Repr]): TraversableViewOps[T, Repr] = expToTravViewExp(t)

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Exp[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]] = expToTravViewExp(
    t.asInstanceOf[Exp[TraversableView[T, C[T]]]])
  //XXX
  implicit def tToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: TraversableView[T, C[_]])(implicit evidence: reflect.ClassTag[C[_]]): TraversableViewOps[T, C[T]] = expToTravViewExp2(t)

  implicit def TraversableExp2ExpTraversable[T](e: Traversable[Exp[T]]): Exp[Traversable[T]] = ExpSeq(e)
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

  implicit def SetForceable[T]: Forceable[T, Set[T]] = new Forceable[T, Set[T]] {
    def force(t: Set[T]) = t
    def force(t: Exp[Set[T]]) = t
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

  trait MapLikeOps[K, V, Repr <: Map[K, V] with MapLike[K, V, Repr]]
    extends Holder[Repr] {
    def get(key: Exp[K]): Exp[Option[V]] = fmap(t, key, 'Map)('get, _ get _)
    /*
    //IterableView[(K, V), Map[K, V]] is not a subclass of Map; therefore we cannot simply return Exp[Map[K, V]].
    case class WithFilter(base: Exp[Map[K, V]], f: Exp[((K, V)) => Boolean]) extends Exp[IterableView[(K, V), Map[K, V]]] {
      override def interpret = base.interpret.view filter f.interpret
    }
    */
  }
  implicit def expToMapLikeOps[K, V, Repr <: Map[K, V] with MapLike[K, V, Repr]](v: Exp[Repr with Map[K, V]]) =
    new MapLikeOps[K, V, Repr] {val t = v}
  implicit def toMapLikeOps[K, V, Repr <: Map[K, V] with MapLike[K, V, Repr]: reflect.ClassTag](v: Repr with Map[K, V]) =
    expToMapLikeOps(v)
}

trait MapOps extends CollectionMapOps {
  this: LiftingConvs with TraversableOps with FunctionOps =>
}

trait IterableOps {
  this: LiftingConvs with TraversableOps =>
}

//Unlike for other collection, Seq by default refers to collection.Seq, not to collection.immutable.Seq
trait CollectionSeqOps {
  this: LiftingConvs with TraversableOps =>

  implicit def CollectionSeqExp2ExpSeq[T](e: Seq[Exp[T]]): Exp[Seq[T]] = ExpSeq(e)
}

trait SeqOps extends CollectionSeqOps {
  this: LiftingConvs with TraversableOps =>
  import collection.immutable.Seq

  implicit def SeqExp2ExpSeq[T](e: Seq[Exp[T]]): Exp[Seq[T]] = ExpSeq(e)
}

trait CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  //We want this lifting to apply to all Sets, not just the immutable ones, so that we can call map also on IncHashSet
  //and get the right type.
  import collection.{SetLike, Set}

  trait SetLikeOps[T, Repr <: Set[T] with SetLike[T, Repr]]
    extends Holder[Repr] {
    def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
    def contains(el: Exp[T]) = apply(el)
    def --(that: Exp[Traversable[T]]): Exp[Repr] =
      Diff(t, that)
  }

  implicit def expToSetLikeOps[T, Repr <: Set[T] with SetLike[T, Repr]](v: Exp[Repr with Set[T]]) =
    new SetLikeOps[T, Repr] {val t = v}
  implicit def toSetLikeOps[T, Repr <: Set[T] with SetLike[T, Repr]: reflect.ClassTag](v: Repr with Set[T]) =
    expToSetLikeOps(v)
  implicit def CollectionSetExp2ExpCollectionSet[T](e: Set[Exp[T]]): Exp[Set[T]] = ExpSeq(e).toSet
}

trait SetOps extends CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  implicit def SetExp2ExpSet[T](e: Set[Exp[T]]): Exp[Set[T]] = ExpSeq(e).toSet
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

  def groupBy[A, K, Repr <: Traversable[A]](coll: Repr with Traversable[A])(f: A => K)(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, A, Repr]): immutable.Map[K, Repr] = {
    val m = mutable.Map.empty[K, Builder[A, Repr]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, cbf(coll) /*coll.companion.newBuilder[A]*/)
      bldr += elem
    }
    val b = immutable.Map.newBuilder[K, Repr]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result withDefault (_ => cbf(coll).result() /*coll.companion.empty[A].asInstanceOf[Repr]*/)
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
  def computeSubTypeRel[T: reflect.ClassTag](seenTypes: Set[Class[_]]): immutable.Map[Class[_], Set[Class[_]]] = {
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

  //Compare collection nodes by identity.
  //Saves costs when comparing collections, which happens during optimization.
  implicit def pureTypeMapping[C[X] <: TraversableLike[X, C[X]], D[+_], T](v: TypeMapping[C, D, T])
                                                                          (implicit cm: reflect.ClassTag[TypeMapping[C, D, T]]) =
    asExp(ConstByIdentity(v)) //asExp here is a simple upcast :-)

  case class GroupByType[T, C[X] <: TraversableLike[X, C[X]], D[+_]](base: Exp[C[D[T]]], f: Exp[D[T] => T])(implicit cbf: CanBuildFrom[C[D[T]], D[T], C[D[T]]], cm: reflect.ClassTag[T]) extends Arity2OpExp[C[D[T]], D[T] => T, TypeMapping[C, D, T],
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
      new TypeMapping[C](x.groupBy[reflect.ClassTag[_]]( (_: Any) => reflect.ClassTag.Int).asInstanceOf[Map[reflect.ClassTag[_], C[_]]])
      //x.groupBy[reflect.ClassTag[_]]( (x:C[T])  => reflect.ClassTag.fromClass(x.getClass).asInstanceOf[reflect.ClassTag[_]])
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
    override def toCode = "%s.get[%s](%s)".format(base.toCode, classS.getName /*XXX won't work*/, Const(classS).toCode)
  }

  class TypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](val t: Exp[C[D[T]]]) {
    def typeFilterWith[S](f: Exp[D[T]] => Exp[T])(implicit cS: reflect.ClassTag[S], cCDS: reflect.ClassTag[C[D[S]]]) =
      TypeFilter[T, C, D, S](t, Fun(f), cS, classManifest[C[D[S]]].toString)
    //def groupByType(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t, Fun(f))
  }
  implicit def expToTypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](t: Exp[C[D[T]]]) = new TypeFilterOps[T, C, D](t)

  class TypeMappingAppOps[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](val t: Exp[TypeMapping[C, D, Base]]) {
    def get[T](implicit cS: reflect.ClassTag[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): TypeMappingApp[C, D, Base, T] = get[T](ClassUtil.boxedErasure(cS))
    def get[T](classS: Class[_])(implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): TypeMappingApp[C, D, Base, T] = TypeMappingApp(t, classS)
  }
  implicit def expToTypeMappingAppOps[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](t: Exp[TypeMapping[C, D, Base]]) = new TypeMappingAppOps[C, D, Base](t)

  //Experiments
  /*
  class GroupByTupleType[U, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](val t: Exp[C[U]]) {
    def groupByTupleType[T, D[_]](typeEqual: U =:= D[T])(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t map (x => fmap(x)('foo, typeEqual)), Fun(f))
  }
  */
  //Copied from Scalaz and altered a bit. Not sure all this generality is useful since we only use it for tuples.
  trait PartialApply1Of2[T[+_, +_], A] {
    type Apply[+B] = T[A, B]
    type Flip[+B] = T[B, A]
  }

  class GroupByTupleTypeOps1[T: reflect.ClassTag, U, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType1(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[T]) */ = GroupByType[T, C, PartialApply1Of2[Tuple2, U]#Flip](this.t, Fun(_._1))
  }
  implicit def expToGroupByTupleType1[T: reflect.ClassTag, U, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps1(t)

  class GroupByTupleTypeOps2[T, U: reflect.ClassTag, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType2(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[U]) */ = GroupByType[U, C, PartialApply1Of2[Tuple2, T]#Apply](this.t, Fun(_._2))
  }
  implicit def expToGroupByTupleType2[T, U: reflect.ClassTag, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps2(t)

  //typeCase method

  //def when[Case, Res](f: Exp[Case] => Exp[Res])(implicit cS: reflect.ClassTag[Case]) = TypeCase(cS, Fun(f))
  trait WhenResult[Case] {
    private[ivm] def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]): TypeCase[Case, Res]
    def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: reflect.ClassTag[Case]): TypeCase[Case, Res]
    def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: reflect.ClassTag[Case]): TypeCase[Case, Res]
  }

  object when {
    def apply[Case] = new WhenResult[Case] {
      private[ivm] override def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]) =
        TypeCase(classS,
          Fun(guard),
          Fun(f))
      override def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: reflect.ClassTag[Case]) =
        onClass[Res](guard, f, ClassUtil.boxedErasure(cS))
      override def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: reflect.ClassTag[Case]) =
        apply(_ => true, f)
    }
  }
}
