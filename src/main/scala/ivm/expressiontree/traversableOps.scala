package ivm.expressiontree

import collection.generic.{CanBuildFrom, GenericTraversableTemplate}
import collection.{GenTraversableView, IterableLike, TraversableView, TraversableViewLike, TraversableLike}
import ivm.collections.TypeMapping

trait TraversableOpsLangIntf {
  this: BaseExpsLangIntf with BaseTypesOpsLangIntf =>

  abstract class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]] {
    def map[U, That <: Traversable[U] with TraversableLike[U, That]](f: Rep[T] => Rep[U])(implicit c: CanBuildFrom[Repr, U, That]): Rep[That]
    def flatMap[U, That <: Traversable[U]](f: Rep[T] => Rep[Traversable[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Rep[That]
    def withFilter(f: Rep[T] => Rep[Boolean]): Rep[Repr]

    def exists(f: Rep[T] => Rep[Boolean]): Rep[Boolean]
    def forall(f: Rep[T] => Rep[Boolean]): Rep[Boolean]

    def collect[U, That <: Traversable[U] with TraversableLike[U, That]](f: Rep[T] => Rep[Option[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Rep[That]

    def filter(f: Rep[T] => Rep[Boolean]): Rep[Repr]

    def union[U >: T, That <: Traversable[U]](that: Rep[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Rep[That]

    // XXX: This cannot be called + to avoid ambiguity with the conversion to NumericOps - probably that's an artifact of it being
    // declared in a subclass
    def :+[U >: T, That <: Traversable[U]](that: Rep[U])(implicit c: CanBuildFrom[Repr, U, That]): Rep[That]
    def ++[U >: T, That <: Traversable[U]](that: Rep[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Rep[That]

    def size: Rep[Int]
    //Omitted @override here because it's inherited.
    //def length = size
    def isEmpty: Rep[Boolean]
    def nonEmpty: Rep[Boolean]

    def view: Rep[TraversableView[T, Repr]]

    def indexBy[K, That](f: Rep[T] => Rep[K])(implicit cbf: CanBuildFrom[Repr, T, That], cTag: ClassTag[T], tTag: TypeTag[T], tTag2: TypeTag[Repr], tTag3: TypeTag[That]): Rep[Map[K, That]]

    def groupBySel[K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](f: Rep[T] => Rep[K], g: Rep[T] => Rep[Rest])
                                                                                       (implicit cbf: CanBuildFrom[Repr, T, Repr],
                                                                                        cbf2: CanBuildFrom[Repr, Rest, That],
                                                                                        cTagT: ClassTag[T],
                                                                                        tTagT: TypeTag[T],
                                                                                        tTagRepr: TypeTag[Repr]): Rep[Map[K, That]]

    def join[S, TKey, TResult, That](innerColl: Rep[Traversable[S]]) //Split argument list to help type inference deduce S and use it after.
                                    (outerKeySelector: Rep[T] => Rep[TKey],
                                     innerKeySelector: Rep[S] => Rep[TKey],
                                     resultSelector: Rep[(T, S)] => Rep[TResult])
                                    (implicit cbf: CanBuildFrom[Repr, TResult, That]): Rep[That]

    def typeFilter[S: ClassTag: TypeTag]: Rep[Traversable[S]]
    private[ivm] def typeFilterClass[S: TypeTag](classS: Class[S]): Rep[Traversable[S]]

    def toSet(implicit tt: TypeTag[T]): Rep[Set[T]]
    def toSeq: Rep[Seq[T]]
    def toList: Rep[List[T]]
    def toVector: Rep[Vector[T]]

    def foldLeft[U](z: Rep[U])(op: Rep[(U, T)] => Rep[U]): Rep[U]
    //fmap(this.t, z, Fun(op), 'TraversableLike)('foldLeft, (base, z, op) => base.foldLeft(z)(Function.untupled(op)))

    def sum[U >: T: ClassTag: TypeTag](implicit num: Numeric[U]): Rep[U]
    def product[U >: T: ClassTag: TypeTag](implicit num: Numeric[U]): Rep[U]
    def head: Rep[T]
    def headOption: Rep[Option[T]]
    def last: Rep[T]
    def lastOption: Rep[Option[T]]

    def typeCase[Res: TypeTag](cases: TypeCase[_, Res]*): Rep[Set[Res]]
  }

  abstract class TraversableTemplateOps[T, +CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]] {
    def flatten[U](implicit asTraversable: T => TraversableOnce[U]): Rep[CC[U]]
    def unzip[T1, T2](implicit asPair: T => (T1, T2)): Rep[(CC[T1], CC[T2])]
  }
  implicit def expToTraversableTemplateOps[T, CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]](v: Rep[CC[T]]): TraversableTemplateOps[T, CC]
  //implicit def toTraversableTemplateOps[T: TypeTag, CC[X] <: Traversable[X] with GenericTraversableTemplate[T, CC]](v: CC[T]) =
    //expToTraversableTemplateOps(v)

  trait TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
    extends TraversableLikeOps[T, ViewColl]
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Rep[That]
  }

  //Compare collection nodes by identity.
  //Saves costs when comparing collections, which happens during optimization.
  implicit def pureColl[T: TypeTag, Repr <: Traversable[T] with TraversableLike[T, Repr]: ClassTag: TypeTag](v: Repr with Traversable[T]): Rep[Repr]

  //This version does not work, due to https://issues.scala-lang.org/browse/SI-5298:
  //implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Rep[Repr])
  /*
  //With this version, we need an extra overload for Range <: IndexedSeq[Int] <: IndexedSeqLike[Int, IndexedSeq[Int]].
  implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Rep[Repr with Traversable[T]]) =
    new TraversableLikeOps[T, Repr](v)
  implicit def toTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Repr with Traversable[T]) =
    expToTraversableLikeOps(v)
  implicit def expRangeToTraversableLikeOps(r: Rep[Range]) = expToTraversableLikeOps(r: Rep[IndexedSeq[Int]])
  */
  /* Instead, use TraversableLike[T, Repr] in the bound, so that it can be considered when looking for a matching Repr;
   * Repr is now deduced by matching against TraversableLike[T, Repr]. At least `with` is still commutative - this works
   * irrespective of the order in which the bounds are considered. */
  implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Rep[Repr with TraversableLike[T, Repr]]): TraversableLikeOps[T, Repr]
  implicit def toTraversableLikeOps[T: TypeTag, Repr <: Traversable[T] with TraversableLike[T, Repr]: ClassTag: TypeTag](v: Repr with TraversableLike[T, Repr]) =
    expToTraversableLikeOps(v)

  //Ranges are cheap to compare for equality; hence we can easily use pure, not pureColl, for them.
  implicit def pureRange(r: Range): Rep[Range] = pure(r)
  //Repr = Range does not satisfy the bound given by:
  //  Repr <: Traversable[T] with TraversableLike[T, Repr]
  //Hence we need this extra conversion.
  implicit def rangeToTraversableLikeOps(r: Range) = expToTraversableLikeOps(r: Rep[IndexedSeq[Int]])

  //XXX: this class is now essentially useless.
  trait TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]]
    extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]]

  implicit def expToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Rep[TraversableView[T, Repr]]): TraversableViewOps[T, Repr]
  implicit def tToTravViewExp[T: TypeTag, Repr <: Traversable[T] with TraversableLike[T, Repr]: ClassTag: TypeTag](t: TraversableView[T, Repr]): TraversableViewOps[T, Repr] = expToTravViewExp(t)

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Rep[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]]
  //XXX
  implicit def tToTravViewExp2[T: TypeTag, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: TraversableView[T, C[_]])(implicit evidence: ClassTag[C[_]], ev2: TypeTag[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]] = expToTravViewExp2(t)

  implicit def TraversableExp2ExpTraversable[T](e: Traversable[Rep[T]]): Rep[Traversable[T]]
}


trait TraversableOps extends TraversableOpsLangIntf {
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
    MapNode[T, Repr, U, That](base, f)
  def newFlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr], f: Fun[T, Traversable[U]])
                                        (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    FlatMap[T, Repr, U, That](base, f)

  def newUnion[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U >: T, That <: Traversable[U]](base: Exp[Repr with Traversable[T]], that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
    Union[U, Repr, That](base, that)

  def groupBySelImpl[T: ClassTag: TypeTag, Repr <: Traversable[T] with
    TraversableLike[T, Repr]: TypeTag, K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                                                                                                    g: Exp[T] => Exp[Rest])(
    implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]]

  trait Holder[Repr] {
    val t: Exp[Repr]
  }

  class TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](protected val t: Exp[Repr]) extends super.TraversableLikeOps[T, Repr] {
    def map[U, That <: Traversable[U] with TraversableLike[U, That]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newMapOp(this.t, Fun(f))
    def flatMap[U, That <: Traversable[U]](f: Exp[T] => Exp[Traversable[U]])
                                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
      newFlatMap(this.t, Fun(f))
    def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
      newWithFilter(this.t, Fun(f))

    def exists(f: Exp[T] => Exp[Boolean]) = !asExp(IsEmpty(this withFilter f))//(withFilter f).isEmpty
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

    override def indexBy[K, That](f: Exp[T] => Exp[K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, That], cTag: ClassTag[T], tTag: TypeTag[T], tTag2: TypeTag[Repr], tTag3: TypeTag[That]): Exp[Map[K, That]] =
      IndexBy[T, Repr, K, That](this.t, Fun(f))

    def groupBySel[K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](f: Exp[T] => Exp[K], g: Exp[T] => Exp[Rest])
                                                                                       (implicit cbf: CanBuildFrom[Repr, T, Repr],
                                                                                        cbf2: CanBuildFrom[Repr, Rest, That],
                                                                                        cTagT: ClassTag[T],
                                                                                        tTagT: TypeTag[T],
                                                                                        tTagRepr: TypeTag[Repr]): Exp[Map[K, That]] =
      groupBySelImpl(this.t, f, g)

    def join[S, TKey, TResult, That](innerColl: Exp[Traversable[S]]) //Split argument list to help type inference deduce S and use it after.
                                    (outerKeySelector: Exp[T] => Exp[TKey],
                                     innerKeySelector: Exp[S] => Exp[TKey],
                                     resultSelector: Exp[(T, S)] => Exp[TResult])
                                    (implicit cbf: CanBuildFrom[Repr, TResult, That]): Exp[That]
    = Join[T,Repr,S,TKey,TResult,That](this.t, innerColl, Fun(outerKeySelector), Fun(innerKeySelector), Fun(resultSelector))

    //def forall(f: Exp[T] => Exp[Boolean]) = Forall(this.t, Fun(f))
    //This awkward form is needed to help type inference - it cannot infer the type of x in `x => !f(x)`.
    //def exists(f: Exp[T] => Exp[Boolean]) = !(Forall(this.t, Fun(f andThen (!(_)))))

    def typeFilter[S: ClassTag: TypeTag]: Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, Fun(identity[Exp[T]]), cS) //variance mismatch
      TypeFilter[T, Traversable, ID, S](t, Fun(identity[Exp[T]]))
    }
    private[ivm] def typeFilterClass[S: TypeTag](classS: Class[S]): Exp[Traversable[S]] = {
      type ID[+T] = T
      //TypeFilter[T, Coll, ID, S](t, Fun(identity[Exp[T]]), classS) //variance mismatch again
      TypeFilter[T, Traversable, ID, S](t, Fun(identity[Exp[T]]), classS)
    }

    def toSetInternal = fmap(this.t, 'TraversableLike)('toSet, _.toSet)
    def toSet(implicit tt: TypeTag[T]) =
      fmap(this.t, 'TraversableLike)(Symbol("toSet[%s]" format (Compile manifestToString tt)), _.toSet)
    //XXX: Generate these wrappers, also for other methods.
    def toSeq = fmap(this.t, 'TraversableLike)('toSeq, _.toSeq)
    def toList = fmap(this.t, 'TraversableLike)('toList, _.toList)
    def toVector = fmap(this.t, 'TraversableLike)('toVector, _.toVector)

    def foldLeft[U](z: Exp[U])(op: Exp[(U, T)] => Exp[U]): Exp[U] = FoldLeft[T, U, Repr](this.t, z, op)
    //fmap(this.t, z, Fun(op), 'TraversableLike)('foldLeft, (base, z, op) => base.foldLeft(z)(Function.untupled(op)))

    def sum[U >: T: ClassTag: TypeTag](implicit num: Numeric[U]): Exp[U] = foldLeft(pure(num.zero))(app(pure((num.plus _).tupled)))
    def product[U >: T: ClassTag: TypeTag](implicit num: Numeric[U]): Exp[U] = foldLeft(pure(num.one))(app(pure((num.times _).tupled)))
    def head: Exp[T] = fmap(this.t, 'TraversableLike)('head, _.head)
    def headOption: Exp[Option[T]] = fmap(this.t, 'TraversableLike)('headOption, _.headOption)
    def last: Exp[T] = fmap(this.t, 'TraversableLike)('last, _.last)
    def lastOption: Exp[Option[T]] = fmap(this.t, 'TraversableLike)('lastOption, _.lastOption)

    def typeCase[Res: TypeTag](cases: TypeCase[_, Res]*): Exp[Set[Res]] = TypeCaseExp(this.t, cases)
  }

  class TraversableTemplateOps[T, +CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]](protected val t: Exp[CC[T]]) extends super.TraversableTemplateOps[T, CC] {
    def flatten[U](implicit asTraversable: T => TraversableOnce[U]): Exp[CC[U]] = fmap(this.t, 'TraversableLikeOps)('flatten, _.flatten)
    def unzip[T1, T2](implicit asPair: T => (T1, T2)): Exp[(CC[T1], CC[T2])] = fmap(t, 'TraversableTemplate)('unzip, _.unzip)
  }
  implicit def expToTraversableTemplateOps[T, CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC]](v: Exp[CC[T]]) =
    new TraversableTemplateOps[T, CC](v)
  //implicit def toTraversableTemplateOps[T: TypeTag, CC[X] <: Traversable[X] with GenericTraversableTemplate[T, CC]](v: CC[T]) =
    //expToTraversableTemplateOps(v)

  class TraversableViewLikeOps[
    T,
    Repr <: Traversable[T] with TraversableLike[T, Repr],
    ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]](tp: Exp[ViewColl])
    extends TraversableLikeOps[T, ViewColl](tp)
  {
    def force[That](implicit bf: CanBuildFrom[Repr, T, That]): Exp[That] = Force[T, Repr, ViewColl, That](this.t)

    //def withFilter(f: Exp[T] => Exp[Boolean]): Exp[ViewColl] =
      //newFilter[T, ViewColl](this.t, Fun(f))
    //TODO: override operations to avoid using CanBuildFrom
  }

  //Compare collection nodes by identity.
  //Saves costs when comparing collections, which happens during optimization.
  override implicit def pureColl[T: TypeTag, Repr <: Traversable[T] with TraversableLike[T, Repr]: ClassTag: TypeTag](v: Repr with Traversable[T]): Exp[Repr] =
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

  //XXX: this class is now essentially useless.
  class TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](tp: Exp[TraversableView[T, Repr]])
    extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]](tp) with super.TraversableViewOps[T, Repr]

  implicit def expToTravViewExp[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[TraversableView[T, Repr]]): TraversableViewOps[T, Repr] = new TraversableViewOps(t)

  implicit def expToTravViewExp2[T, C[X] <: Traversable[X] with TraversableLike[X, C[X]]](t: Exp[TraversableView[T, C[_]]]): TraversableViewOps[T, C[T]] = expToTravViewExp(
    t.asInstanceOf[Exp[TraversableView[T, C[T]]]])

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
    def force(t: Exp[TraversableView[T, Traversable[_]]]) =
      expToTravViewExp2(t).force
      //t.force //Fails compilation on 2.10.2. The same error appears on tests.
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
  implicit def toMapLikeOps[K: TypeTag, V: TypeTag, Repr <: Map[K, V] with MapLike[K, V, Repr]: ClassTag: TypeTag](v: Repr with Map[K, V]) =
    expToMapLikeOps(v)
}

trait MapOps extends CollectionMapOps {
  this: LiftingConvs with TraversableOps with FunctionOps =>
}

trait IterableOps {
  this: LiftingConvs with TraversableOps with FunctionOps =>
  implicit def expToIterableLikeOps[T, Repr <: Iterable[T] with IterableLike[T, Repr]](v: Exp[Repr with IterableLike[T, Repr]]) =
    new IterableLikeOps[T, Repr] {val t = v}
  trait IterableLikeOps[T, Repr <: Iterable[T] with IterableLike[T, Repr]]
    extends Holder[Repr] {
    def zipWithIndex[T1 >: T, That](implicit bf: CanBuildFrom[Repr, (T1, Int), That]): Exp[That] = fmap(this.t, 'IterableLike)('zipWithIndex, _.zipWithIndex)
    def sliding(size: Exp[Int]): Exp[Seq[Repr]] = fmap(this.t, size, 'IterableLike)('sliding, (coll, size) => (coll sliding size).toList)
  }
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
  implicit def toSetLikeOps[T: TypeTag, Repr <: Set[T] with SetLike[T, Repr]: ClassTag: TypeTag](v: Repr with Set[T]) =
    expToSetLikeOps(v)
  implicit def CollectionSetExp2ExpCollectionSet[T: TypeTag](e: Set[Exp[T]]): Exp[Set[T]] = ExpSeq(e).toSet
}

trait SetOps extends CollectionSetOps {
  this: LiftingConvs with TraversableOps =>
  implicit def SetExp2ExpSet[T: TypeTag](e: Set[Exp[T]]): Exp[Set[T]] = ExpSeq(e).toSet
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
import collection.mutable.{Queue, ArrayBuffer, Builder}

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

  def groupBy[A, K, Repr <: Traversable[A], That](coll: Repr with Traversable[A])(f: A => K)(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, A, That]): immutable.Map[K, That] = {
    val m = mutable.Map.empty[K, Builder[A, That]]
    for (elem <- coll) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, cbf(coll) /*coll.companion.newBuilder[A]*/)
      bldr += elem
    }
    val b = immutable.Map.newBuilder[K, That]
    for ((k, v) <- m)
      b += ((k, v.result()))

    val emptyColl = cbf(coll).result()  /*coll.companion.empty[A].asInstanceOf[Repr]*/
    //Don't inline emptyColl - currently, the result holds onto emptyColl; after inlining, it would hold onto the potentially much bigger coll.
    b.result withDefault (_ => emptyColl)
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
  def computeSubTypeRel[T: ClassTag](seenTypes: Set[Class[_]]): immutable.Map[Class[_], Set[Class[_]]] = {
    //val subtypeRel = mutable.Set.empty[(Class[_], Class[_])]
    val subtypeRel = ArrayBuffer.empty[(Class[_], Class[_])]
    val classesToScan: Queue[Class[_]] = Queue()
    def add(clazz: Class[_]) {
      val superTypesClazz = superTypes(clazz)
      classesToScan enqueue (superTypesClazz: _*)
      for (superType <- superTypesClazz)
        subtypeRel += (superType -> clazz) //Map s to its subtypes.
    }
    val erasedT = ClassUtil.boxedErasure(classTag[T])
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
                                                                          (implicit cm: ClassTag[TypeMapping[C, D, T]],
                                                                           tTag: TypeTag[TypeMapping[C, D, T]]) =
    asExp(ConstByIdentity(v)) //asExp here is a simple upcast :-)

  case class GroupByType[T, C[X] <: TraversableLike[X, C[X]], D[+_]](base: Exp[C[D[T]]], f: Exp[D[T] => T])(implicit cbf: CanBuildFrom[C[D[T]], D[T], C[D[T]]], cm: ClassTag[T]) extends Arity2OpExp[C[D[T]], D[T] => T, TypeMapping[C, D, T],
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
    //XXX add toCode
  }

  /*
   * failed attempt to code GroupByType without type cast
    case class GroupByType[T, C[X] <: Traversable[X], Repr <: TraversableLike[T, Repr]](base: Exp[C[T] with Repr]) extends Arity1OpExp[C[T] with Repr, TypeMapping[C]](base) {
    override def interpret() = {
      val x: C[T] with Repr = base.interpret()
      new TypeMapping[C](x.groupBy[ClassTag[_]]( (_: Any) => ClassTag.Int).asInstanceOf[Map[ClassTag[_], C[_]]])
      //x.groupBy[ClassTag[_]]( (x:C[T])  => ClassTag.fromClass(x.getClass).asInstanceOf[ClassTag[_]])
    }
    override def copy(base: Exp[C[T] with Repr]) = GroupByType[T, C, Repr](base)
  }
 */
  case class TypeMappingApp[C[+X] <: TraversableLike[X, C[X]], D[+_], Base, T](base: Exp[TypeMapping[C, D, Base]], classS: Class[_])
                                                                              (implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]])
    extends Arity1OpExp[TypeMapping[C, D, Base], C[D[T]], TypeMappingApp[C, D, Base, T]](base) with PersistClassS[C[D[T]]] {
    override def copy(base: Exp[TypeMapping[C, D, Base]]) = TypeMappingApp[C, D, Base, T](base, classS)
    override def interpret() =
      base.interpret().get[T](classS)
    override def toCode = "%s.get[%s](%s)".format(base.toCode, classS.getName /*XXX won't work*/, persistedValue)
  }

  class TypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](val t: Exp[C[D[T]]]) {
    def typeFilterWith[S](f: Exp[D[T]] => Exp[T])(implicit cS: ClassTag[S], cCDS: TypeTag[C[D[S]]]) =
      TypeFilter[T, C, D, S](t, Fun(f))
    //def groupByType(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t, Fun(f))
  }
  implicit def expToTypeFilterOps[T, C[+X] <: TraversableLike[X, C[X]], D[+_]](t: Exp[C[D[T]]]) = new TypeFilterOps[T, C, D](t)

  class TypeMappingAppOps[C[+X] <: TraversableLike[X, C[X]], D[+_], Base](val t: Exp[TypeMapping[C, D, Base]]) {
    def get[T](implicit cS: ClassTag[T], m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): Exp[C[D[T]]] = get[T](ClassUtil.boxedErasure(cS))
    def get[T](classS: Class[_])(implicit m: MaybeSub[Base, T], cbf: CanBuildFrom[C[D[Base]], D[T], C[D[T]]]): Exp[C[D[T]]] = TypeMappingApp[C, D, Base, T](t, classS)
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

  class GroupByTupleTypeOps1[T: ClassTag, U, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType1(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[T]) */ = GroupByType[T, C, PartialApply1Of2[Tuple2, U]#Flip](this.t, toAtomImplicit(Fun(_._1)))
  }
  implicit def expToGroupByTupleType1[T: ClassTag, U, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps1(t)

  class GroupByTupleTypeOps2[T, U: ClassTag, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[(T, U)]]) {
    def groupByTupleType2(implicit cbf: CanBuildFrom[C[(T, U)], (T, U), C[(T, U)]]) /*(f: Exp[(T, U)] => Exp[U]) */ = GroupByType[U, C, PartialApply1Of2[Tuple2, T]#Apply](this.t, toAtomImplicit(Fun(_._2)))
  }
  implicit def expToGroupByTupleType2[T, U: ClassTag, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[(T, U)]]) = new GroupByTupleTypeOps2(t)

  //typeCase method

  //def when[Case, Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassTag[Case]) = TypeCase(cS, Fun(f))
  trait WhenResult[Case] {
    private[ivm] def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]): TypeCase[Case, Res]
    def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassTag[Case]): TypeCase[Case, Res]
    def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: ClassTag[Case]): TypeCase[Case, Res]
  }

  object when {
    def apply[Case] = new WhenResult[Case] {
      private[ivm] override def onClass[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res], classS: Class[_]) =
        TypeCase(classS,
          Fun(guard),
          Fun(f))
      override def apply[Res](guard: Exp[Case] => Exp[Boolean], f: Exp[Case] => Exp[Res])(implicit cS: ClassTag[Case]) =
        onClass[Res](guard, f, ClassUtil.boxedErasure(cS))
      override def apply[Res](f: Exp[Case] => Exp[Res])(implicit cS: ClassTag[Case]) =
        apply(_ => true, f)
    }
  }
}
