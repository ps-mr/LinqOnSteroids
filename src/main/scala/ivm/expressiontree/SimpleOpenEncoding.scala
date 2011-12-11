package ivm.expressiontree

import collection.generic.{FilterMonadic, CanBuildFrom}
import collection.{TraversableView, TraversableViewLike, IterableView, TraversableLike, mutable}
import ivm.collections.TypeMapping

/**
 * Here I show yet another encoding of expression trees, where methods
 * like +, &lt;= and so on can be added by other classes, rather than having to
 * be inserted in the original object.
 *
 * The original encoding of Klaus had this property but relied on an implicit
 * conversion from T to Exp[T] and then on additional ones from Exp[Double] to
 * DoubleExp, Exp[String] to StringExp, and so on. Quite a few times this did
 * not work because Scala never applies two implicit conversions on top of one
 * another.
 * This can be solved by making their composition available as another implicit conversion, and that's the solution
 * we show here.
 */
object SimpleOpenEncoding {
  trait ConversionDisabler {
    //We forbid implicit conversion from Unit to Exp[Unit] by making it ambiguous. To this end we declare noToExpForUnit.
    //It is more specific than toExp[Unit] because it's not generic, but is declared in a superclass, hence
    //has less priority. Ambiguity follows.
    implicit def noToExpForUnit(t: Unit): Exp[Unit] = null
    //Ditto. Creating Const nodes for mutable collection is a contradiction; moreover, those nodes would send no
    //notification for updates to the underlying collection.
    //To test, edit testNoMutableConst below to see that the currently commented-out code does not compile.
    implicit def noConstForMutableColl[T](t: mutable.Traversable[T]): Exp[mutable.Traversable[T]] = null
  }

  trait OpsExpressionTreeTrait extends ConversionDisabler {
    implicit def toExp[T](t: T): Exp[T] = Const(t)
    /*implicit def liftOrd[T: Ordering](x: T) = Const(x)
    implicit def liftNum[T: Numeric](x: T) = Const(x)

    implicit def liftBool(x: Boolean): Exp[Boolean] = Const(x)
    implicit def liftString(x: String): Exp[String] = Const(x)*/

    implicit def pairToPairExp[A, B](pair: (Exp[A], Exp[B])): Pair[A, B] = Pair[A, B](pair._1, pair._2)

    //To "unlift" a pair, here's my first solution:
    /*implicit*/ def unliftPair[A, B](pair: Exp[(A, B)]): (Exp[A], Exp[B]) = (Proj1(pair), Proj2(pair))
    /*
    //Unfortunately this conversion is not redundant; we may want to have a special node to support this, or to
    //remove Pair constructors applied on top of other pair constructors.
    implicit def expPairToPairExp[A, B](pair: Exp[(A, B)]): Pair[A, B] =
      (Pair[A, B] _).tupled(unliftPair(pair))
    */

    //Here's the second one, adapted from Klaus code. It represents but does not build a tuple (once one adds lazy vals).
    //However, one cannot do pattern matching against the result, not with the existing pattern.
    //Lesson: Scala does not allow to define additional extractors for a given pattern type, and syntax shortcuts such
    //as tuples or => are simply built-in in the language.
    case class PairHelper[A, B](p: Exp[(A, B)]) {
      lazy val _1 = Proj1(p)
      lazy val _2 = Proj2(p)
    }

    implicit def toPairHelper[A, B](e: Exp[(A, B)]): PairHelper[A, B] = PairHelper(e)

    implicit def fToFunOps[A, B](f: Exp[A => B]): Exp[A] => Exp[B] =
      f match {
        case FuncExp(fe) => fe(_) //This line should be dropped, but then we'll need to introduce a beta-reducer.
                                  // KO: Why do we need a beta-reducer? Since we use HOAS this is just Scala function application
                                  // and already available in App.interpret
                                  // But it may still make sense to evaluate such applications right away
                                  // PG: I believe we need a beta-reducer before any optimization, to ensure that beta-equivalent
                                  // operations optimize to the same thing. Otherwise the optimizer might not find a pattern
                                  // because it would show up only after reduction.
                                  // I believe we want to have App for the same exact reason not all function calls are
                                  // inlined: preventing code size explosion.
        case _ => App(f, _)
      }

    // these functions are explicitly not implicit :)
    def liftCall[Res](id: Symbol, callfunc: () => Res) = new Call0(id, callfunc)
    def liftCall[A0, Res](id: Symbol, callfunc: A0 => Res, arg0: Exp[A0]) = new Call1(id, callfunc, arg0)
    def liftCall[A0, A1, Res](id: Symbol, callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) =
      new Call2(id, callfunc, arg0, arg1)
    def liftCall[A0, A1, A2, Res](id: Symbol, callfunc: (A0, A1, A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) =
      new Call3(id, callfunc, arg0, arg1, arg2)
    def liftCall[A0, A1, A2, A3, Res](id: Symbol, callfunc: (A0, A1, A2, A3) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3]) =
      new Call4(id, callfunc, arg0, arg1, arg2, arg3)
    def liftCall[A0, A1, A2, A3, A4, Res](id: Symbol, callfunc: (A0, A1, A2, A3, A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) =
      new Call5(id, callfunc, arg0, arg1, arg2, arg3, arg4)

    def onExp[A0, Res](t: Exp[A0])(id: Symbol, f: A0 => Res): Exp[Res] = liftCall(id, f, t)
    def onExp[A0, A1, Res](a0: Exp[A0], a1: Exp[A1])(id: Symbol, f: (A0, A1) => Res): Exp[Res] = liftCall(id, f, a0, a1)
    def onExp[A0, A1, A2, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2])(id: Symbol, f: (A0, A1, A2) => Res): Exp[Res] = liftCall(id, f, a0, a1, a2)
    def onExp[A0, A1, A2, A3, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3])(id: Symbol, f: (A0, A1, A2, A3) => Res): Exp[Res] =
      liftCall(id, f, a0, a1, a2, a3)
    def onExp[A0, A1, A2, A3, A4, Res](a0: Exp[A0], a1: Exp[A1], a2: Exp[A2], a3: Exp[A3], a4: Exp[A4])(id: Symbol, f: (A0, A1, A2, A3, A4) => Res): Exp[Res] =
      liftCall(id, f, a0, a1, a2, a3, a4)
  }
  object OpsExpressionTree extends OpsExpressionTreeTrait

  /**
   * In comparison to the other encoding, we don't use CanBuildExp to get most specific types as result types, as
   * that implies that the type is obtained through type inference.
   * Instead, we use conversions from T => Exp[T], and
   * then specific ones Pi T: Numeric. Exp[T] => NumExp[T]; Pi T. Exp[Traversable[T]] => TraversableExp[T]
   */

  object NumOpsExps {
    import OpsExpressionTree._
    class NumOps[T: Numeric](val t: Exp[T]) {
      def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    }
    class OrderingOps[T: Ordering](t: Exp[T]) {
      def <=(that: Exp[T]) = LEq(t, that)
    }

    implicit def expToNumOps[T : Numeric](t: Exp[T]): NumOps[T] = new NumOps(t)
    implicit def tToNumOps[T: Numeric](t: T): NumOps[T] = expToNumOps(t)
    implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
    implicit def tToOrderingOps[T: Ordering](t: T) = expToOrderingOps(t)
  }

  trait TraversableOps {
    import OpsExpressionTree._
    def newWithFilter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr],
                                                        f: FuncExp[T, Boolean]) =
      new FilterMaintainerExp(View[T, Repr](base), f)
    def newMapOp[T, Repr <: Traversable[T] with TraversableLike[T, Repr], U, That <: Traversable[U]](base: Exp[Repr],
                                                             f: FuncExp[T, U])
                                                            (implicit c: CanBuildFrom[Repr, U, That]) =
      new MapOpMaintainerExp[T, Repr, U, That](base, f)

    /* Lift faithfully the FilterMonadic trait except foreach and withFilter, since we have a special lifting for it.
     * This trait is used both for concrete collections of type Repr <: FilterMonadic[T, Repr].
     */
    trait FilterMonadicOpsLike[T, Repr <: Traversable[T] with TraversableLike[T, Repr]] {
      val t: Exp[Repr]
      def map[U, That <: Traversable[U]](f: Exp[T] => Exp[U])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        new MapOpMaintainerExp[T, Repr, U, That](this.t, FuncExp(f))
      def map2[U, That <: Traversable[U]](f: T => U)(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        new MapOpMaintainerExp[T, Repr, U, That](this.t, FuncExp(f: Exp[T => U]))
      def flatMap[U, That <: Traversable[U]](f: Exp[T] => Exp[TraversableOnce[U]])
                          (implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        new FlatMapMaintainerExp[T, Repr, U, That](this.t, FuncExp(f))
    }

    case class GroupBy[T, Repr <: TraversableLike[T, Repr], K](base: Exp[Repr], f: Exp[T => K]) extends BinaryOpExp[Repr,
      T => K, Map[K, Repr]](base, f) {
      override def interpret = base.interpret groupBy f.interpret()
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

    trait TraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]] extends FilterMonadicOpsLike[T, Repr] {
      def collect[U, That <: Traversable[U]](f: Exp[T] => Exp[Option[U]])
                   (implicit c: CanBuildFrom[TraversableView[T, Repr], U, That]): Exp[That] = {
         new MapOpMaintainerExp(new FilterMaintainerExp[T, TraversableView[T, Repr]](View[T, Repr](this.t),
           FuncExp((x: Exp[T]) => IsDefinedAt(PartialFuncExp(f), x))),
           FuncExp((x: Exp[T]) => App(PartialFuncExp(f), x)))(c)
      }

      def filter(f: Exp[T] => Exp[Boolean]): Exp[Repr] =
        new FilterMaintainerExp(this.t, FuncExp(f))

      def union[U >: T, That <: Traversable[U]](that: Exp[Traversable[U]])(implicit c: CanBuildFrom[Repr, U, That]): Exp[That] =
        new UnionMaintainerExp[U, Repr, That](this.t, that)

      def view: Exp[TraversableView[T, Repr]] = View[T, Repr](this.t)

      def groupBy[K](f: Exp[T] => Exp[K]): Exp[Map[K, Repr]] =
        GroupBy(this.t, FuncExp(f))

      def join[S, TKey, TResult, That](innerColl: Exp[Traversable[S]]) //Split argument list to help type inference deduce S and use it after.
                                      (outerKeySelector: Exp[T] => Exp[TKey],
                                       innerKeySelector: Exp[S] => Exp[TKey],
                                       resultSelector: Exp[(T, S)] => Exp[TResult])
                                      (implicit cbf: CanBuildFrom[Repr, TResult, That]): Exp[That]
      = Join(this.t, innerColl, FuncExp(outerKeySelector), FuncExp(innerKeySelector), FuncExp(resultSelector))
    }

    trait TraversableViewLikeOps[
        T,
        Repr <: Traversable[T] with TraversableLike[T, Repr],
        ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl]]
      extends TraversableLikeOps[T, ViewColl] with WithFilterable[T, Repr]
    {
      def force[That](implicit bf: CanBuildFrom[Repr, T, That]) = Force[T, Repr, ViewColl, That](this.t)

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[ViewColl] =
        new FilterMaintainerExp[T, ViewColl](this.t, FuncExp(f))
    }

    class TraversableOps[T](val t: Exp[Traversable[T]]) extends TraversableLikeOps[T, Traversable[T]] with WithFilterImpl[T, Traversable[T], Traversable[T]]

    class TraversableViewOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](val t: Exp[TraversableView[T, Repr]])
      extends TraversableViewLikeOps[T, Repr, TraversableView[T, Repr]]

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

  /**
   * A goal of this new encoding is to be able to build expression trees (in particular, query trees) producing
   * different collections; once we can represent query trees producing maps and maintain them incrementally, view
   * maintenance can subsume index update.
   */

  //XXX: we'll probably have to duplicate this for Maps, as for Sets below.
  trait MapOps extends TraversableOps {
    import OpsExpressionTree._
    class MapOps[K, V](val t: Exp[Map[K, V]]) extends TraversableLikeOps[(K, V), Map[K, V]] with WithFilterImpl[(K, V), Map[K, V], Map[K, V]] {
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

  trait CollectionSetOps extends TraversableOps {
    import OpsExpressionTree.toExp
    //We want this lifting to apply to all Sets, not just the immutable ones, so that we can call map also on IncHashSet
    //and get the right type.
    import collection.Set

    case class Contains[T](set: Exp[Set[T]], v: Exp[T]) extends BinaryOpExp[Set[T], T, Boolean](set, v) {
      def interpret() = set.interpret().contains(v.interpret())
      def copy(set: Exp[Set[T]], v: Exp[T]) = Contains(set: Exp[Set[T]], v: Exp[T])
    }

    class CollectionSetOps[T](val t: Exp[Set[T]]) extends TraversableLikeOps[T, Set[T]] with WithFilterImpl[T, Set[T], Set[T]] {
      def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
      def contains(el: Exp[T]) = apply(el)
    }
    implicit def expToCollectionSetExp[T](t: Exp[Set[T]]): CollectionSetOps[T] = new CollectionSetOps(t)
    implicit def tToCollectionSetExp[T](t: Set[T]): CollectionSetOps[T] = expToCollectionSetExp(t)
  }

  trait SetOps extends CollectionSetOps {
    import OpsExpressionTree.toExp
    //For convenience, also have a lifting for scala.Set = scala.collection.immutable.Set.

    class SetOps[T](val t: Exp[Set[T]]) extends TraversableLikeOps[T, Set[T]] with WithFilterImpl[T, Set[T], Set[T]] {
      def apply(el: Exp[T]): Exp[Boolean] = Contains(t, el)
      def contains(el: Exp[T]) = apply(el)
    }
    implicit def expToSetExp[T](t: Exp[Set[T]]): SetOps[T] = new SetOps(t)
    implicit def tToSetExp[T](t: Set[T]): SetOps[T] = expToSetExp(t)
  }

  trait TypeFilterOps extends TraversableOps {
    import OpsExpressionTree._
    case class GroupByType[T, C[X] <: TraversableLike[X, C[X]], D[_]](base: Exp[C[D[T]]], f: Exp[D[T] => T]) extends BinaryOpExp[C[D[T]], D[T] => T, TypeMapping[C, D]](base, f) {
      override def interpret() = {
        val x: C[D[T]] = base.interpret()
        val g: D[T] => T = f.interpret()

        new TypeMapping[C, D](x.groupBy
          ((x: D[T] /* T */) => ClassManifest.fromClass(g(x).getClass)).asInstanceOf[Map[ClassManifest[_], C[D[_]]]])
      }
      override def copy(base: Exp[C[D[T]]], f: Exp[D[T]=>T]) = GroupByType[T, C, D](base, f)
    }

    /*
     * failed attempt to code GroupByType without type cast
      case class GroupByType[T, C[X] <: Traversable[X], Repr <: TraversableLike[T, Repr]](base: Exp[C[T] with Repr]) extends UnaryOpExp[C[T] with Repr, TypeMapping[C]](base) {
      override def interpret = {
        val x: C[T] with Repr = base.interpret()
        new TypeMapping[C](x.groupBy[ClassManifest[_]]( (_: Any) => ClassManifest.Int).asInstanceOf[Map[ClassManifest[_], C[_]]])
        //x.groupBy[ClassManifest[_]]( (x:C[T])  => ClassManifest.fromClass(x.getClass).asInstanceOf[ClassManifest[_]])
      }
      override def copy(base: Exp[C[T] with Repr]) = GroupByType[T, C, Repr](base)
    }
   */
    case class TypeMappingApp[C[X] <: TraversableLike[X, C[X]], D[_], S](base: Exp[TypeMapping[C, D]])(implicit cS: ClassManifest[S])
       extends UnaryOpExp[TypeMapping[C, D], C[D[S]]](base) {
      override def copy(base: Exp[TypeMapping[C, D]]) = TypeMappingApp[C, D, S](base)
      override def interpret = {
        base.interpret.get[S]
      }

    }
    class TypeFilterOps[T, C[X] <: TraversableLike[X, C[X]], D[_]](val t: Exp[C[D[T]]]) {
      def typeFilterWith[S](f: Exp[D[T]] => Exp[T])(implicit cS: ClassManifest[S]) = TypeFilter[T, C, D, S](t, FuncExp(f))
      def groupByType(f: Exp[D[T]] => Exp[T]) = GroupByType(this.t, FuncExp(f))
    }
    class SimpleTypeFilterOps[T, C[X] <: TraversableLike[X, C[X]]](val t: Exp[C[T]]) {
      type ID[T] = T
      def typeFilter[S](implicit cS: ClassManifest[S]) = TypeFilter[T, C, ID, S](t, FuncExp(identity))
    }
    class TypeMappingAppOps[C[X] <: TraversableLike[X, C[X]], D[_]](val t: Exp[TypeMapping[C, D]]) {
      def get[S](implicit cS: ClassManifest[S]) = TypeMappingApp[C, D, S](t)
    }
    implicit def expToTypeFilterOps[T, C[X] <: TraversableLike[X, C[X]], D[_]](t: Exp[C[D[T]]]) = new TypeFilterOps[T, C, D](t)
    implicit def expToSimpleTypeFilterOps[T, C[X] <: TraversableLike[X, C[X]]](t: Exp[C[T]]) = new SimpleTypeFilterOps[T, C](t)
    implicit def expToTypeMappingAppOps[C[X] <: TraversableLike[X, C[X]], D[_]](t: Exp[TypeMapping[C, D]]) = new TypeMappingAppOps[C, D](t)
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
}
