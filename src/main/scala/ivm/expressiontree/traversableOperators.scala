package ivm.expressiontree

import collection.generic.{CanBuildFrom, FilterMonadic}
import collection._

/**
* User: pgiarrusso
* Date: 19/9/2011
*/

// It's amazing that Scala accepts "extends Exp[That]", since it would not accept That; most probably that's thanks to erasure.
//I believe that in pattern matching, base will be deduced to have type Exp[Repr] which erases to Exp[Any] because the type bounds are not
//considered well-enough.
case class FlatMap[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                   U, That <: Traversable[U]](base: Exp[Repr with Traversable[T]], f: Fun[T, Traversable[U]])
                            (implicit /*protected[this]*/ val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[Repr], Fun[T, Traversable[U]], That, FlatMap[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() flatMap f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Traversable[U]]) = FlatMap[T, Repr, U, That](base, f)
}

case class MapNode[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 U, That <: Traversable[U] with TraversableLike[U, That]](base: Exp[Repr with Traversable[T]], f: Fun[T, U])
                          (implicit /*protected[this] */val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[Repr], Fun[T, U], That, MapNode[T, Repr, U, That]](base, f) {
  override def interpret() = base.interpret() map f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, U]) = MapNode[T, Repr, U, That](base, f)
}

case class Filter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with Traversable[T]],
                                                      f: Fun[T, Boolean]) extends Arity2Op[Exp[Repr], Fun[T, Boolean], Repr, Filter[T, Repr]](base, f) {
  override def interpret() = base.interpret() filter f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Boolean]) = Filter(base, f)
 }

case class WithFilter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: Fun[T, Boolean])
                                                      extends Arity2Op[Exp[Repr], Fun[T, Boolean], TraversableView[T, Repr], WithFilter[T, Repr]](base, f) {
  override def interpret() = base.interpret().view filter f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Boolean]) = WithFilter[T, Repr](base, f)
}

case class View[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with Traversable[T] with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, TraversableView[T, Repr], View[T, Repr]](base) {
  override def interpret() = base.interpret().view
  override def copy(base: Exp[Repr]) = View(base)
}

case class Force[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl], That]
                (base: Exp[ViewColl with TraversableViewLike[T, Repr, ViewColl]])
                (implicit protected[this] val bf: CanBuildFrom[Repr, T, That]) extends Arity1OpExp[ViewColl, That, Force[T, Repr, ViewColl, That]](base) {
  override def interpret() = base.interpret().force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl, That](base)
}

//A bit of a hack, since it does not return the most precise type possible.
case class ForceIfPossible[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, Traversable[T], ForceIfPossible[T, Repr]](base) {
  override def interpret() = {
    Lifting.TraversableForceable.force(base.interpret())
  }
  override def copy(base: Exp[Repr]) = ForceIfPossible(base)
}

case class Union[T, Repr <: TraversableLike[T, Repr], That](base: Exp[Repr], that: Exp[Traversable[T]])
                                   (implicit protected[this] val c: CanBuildFrom[Repr, T, That]) extends Arity2OpExp[Repr, Traversable[T], That, Union[T, Repr, That]](base, that) {
  override def interpret() = base.interpret() ++ that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Union[T, Repr, That](base, that)
}

case class Diff[T, Repr <: collection.Set[T] with SetLike[T, Repr]](base: Exp[Repr], that: Exp[Traversable[T]]) extends Arity2OpExp[Repr, Traversable[T], Repr, Diff[T, Repr]](base, that) {
  override def interpret() = base.interpret() -- that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Diff[T, Repr](base, that)
}

case class Size[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Int, Size[T, Repr]](t) {
  def interpret() = t.interpret().size
  def copy(t: Exp[Repr]) = Size(t)
}

case class IsEmpty[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Boolean, IsEmpty[T, Repr]](t) {
  def interpret() = t.interpret().isEmpty
  def copy(t: Exp[Repr]) = IsEmpty(t)
}

object TypeFilter {
  def apply[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T], cS: ClassManifest[S]) =
    apply[T, C, D, S](base, f, ClassUtil.boxedErasure(cS))
}

//Just like for IfInstanceOf, equality comparison must consider also classS. Therefore, classS must be a class parameter.
case class TypeFilter[T, C[+X] <: TraversableLike[X, C[X]], D[+_], S /* is this too strict? <: T */](base: Exp[C[D[T]]], f: Exp[D[T] => T], classS: Class[_])
                                  extends Arity2Op[Exp[C[D[T]]], Exp[D[T] => T], C[D[S]], TypeFilter[T, C, D, S]](base, f) {
  override def interpret() = {
    val b: C[D[T]] = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[C[D[S]]]
  }
  override def copy(base: Exp[C[D[T]]], f: Exp[D[T] => T]) = TypeFilter[T, C, D, S](base, f, classS)
}

// XXX: It is not clear whether the cast from Repr to That is always valid. OTOH, this could express typeFilter on Map,
// though not necessarily with a desirable interface.
case class TypeFilter2[T, D[+_], Repr <: TraversableLike[D[T], Repr], S, That](base: Exp[Repr],
                                                                               f: Exp[D[T] => T])(implicit cS: ClassManifest[S],
                                                                                                  cb: CanBuildFrom[Repr, S, That])
  extends Arity2Op[Exp[Repr], Exp[D[T] => T], That, TypeFilter2[T, D, Repr, S, That]](base, f)
{
  private[this] val classS = ClassUtil.boxedErasure(cS)
  def interpret() = {
    val b: Repr = base.interpret()
    b.filter(x => classS.isInstance(f.interpret()(x))).asInstanceOf[That]
  }
  def copy(base: Exp[Repr], f: Exp[D[T] => T]) = TypeFilter2[T, D, Repr, S, That](base, f)
}

case class TypeCase[Case, +Res](classS: Class[_], guard: Fun[Case, Boolean], f: Fun[Case, Res]) {
  //The setters for this field use Res in contravariant position, hence it is important to make them private.
  private[this] var _guardInt: Case => Boolean = _
  private[this] var _fInt: Case => Res = _
  private[expressiontree] def guardInt: Case => Boolean = _guardInt
  private[expressiontree] def fInt: Case => Res = _fInt
  private[expressiontree] def preInterpret() {
    _guardInt = guard.interpret()
    _fInt = f.interpret()
  }
}

//The implementation of this function relies on details of erasure for performance:
//- We use null instead of relying on Option, but we filter null values away. In theory this is only allowed if Res >: Null
//that is Res <: AnyRef; this is valid for all types but Res <: AnyVal, i.e. for primitive types, but since Res is a type
//parameter, it will be erased to java.lang.Object and even primitive types will be passed boxed.
//Hence in practice v: Res can be casted to AnyRef and compared against null.
case class TypeCaseExp[BaseT, Repr <: TraversableLike[BaseT, Repr], Res, +That /*XXX to drop*/](e: Exp[Repr with TraversableLike[BaseT, Repr]], cases: Seq[TypeCase[_ /*Case_i*/, Res]])/*(implicit protected[this] val c: CanBuildFrom[TraversableView[BaseT, Repr], Res, That])*/ extends Exp[immutable.Set[Res]] {
  override def nodeArity = 2 * cases.length + 1
  override def children = e +: (cases.flatMap /*[Exp[_], Seq[Exp[_]]] */(c => Seq[Exp[_]](c.guard, c.f)))
  override def checkedGenericConstructor: Seq[Exp[_]] => Exp[immutable.Set[Res]] =
    v => TypeCaseExp(
      v.head.asInstanceOf[Exp[Repr]],
      (cases, v.tail.grouped(2).toSeq).zipped map {case (tc, Seq(guard, f)) => TypeCase(tc.classS, guard.asInstanceOf[Fun[Any, Boolean]], f.asInstanceOf[Fun[Any, Res]])})

  private def checkF(v: BaseT): Res = {
    for (t: TypeCase[Any, Res] <- cases.asInstanceOf[Seq[TypeCase[Any, Res]]]) {
      if (t.classS.isInstance(v) && t.guardInt(v))
        return t.fInt(v).asInstanceOf[Res]
    }
    null.asInstanceOf[Res]
  }
  override def interpret() = {
    //Since cases can contain open terms, preInterpret() must be called at each call of interpret() - the environment might be different and we might thus get different
    //results. We needn't call them once per element of e, since TypeCaseExp binds no variable iterating over e.
    cases foreach (_ preInterpret())
    (e.interpret().view map checkF filter (_.asInstanceOf[AnyRef] ne null)).toSet
  }
  //cases map { case TypeCase(classS, f) => (v: Base) => if (v == null || !classS.isInstance(v)) Util.ifInstanceOfBody(v, classS)}
}


//Note: this class also handles IVM, though in an incomplete way
case class Forall[T](coll: Exp[Traversable[T]], f: Fun[T, Boolean])
  extends Arity1OpExp[Traversable[T], Boolean, Forall[T]](coll) with EvtTransformerEl[Traversable[T], Boolean, Traversable[T]]
  with ExpWithCache[Boolean]
{
  var countFalse: Int = 0
  override def interpret() = {
    //XXX: we should get the initial status otherwise.
    countFalse = coll.interpret().count(x => !f.interpret()(x))
    expResult()
  }

  override def copy(coll: Exp[Traversable[T]]) = Forall(coll, f)

  override def cache = Some(expResult()) //We probably need to make the cache _field_ optional.
  override def expResult() = countFalse == 0 //The result is always valid here.

  override def notifyEv(pub: Traversable[T], evt: Message[Traversable[T]]) {
    evt match {
      case Include(v) =>
        countFalse += (if (!f.interpret()(v)) 1 else 0)
      case Remove(v) =>
        countFalse -= (if (!f.interpret()(v)) 1 else 0)
      case Update(oldV, newV) =>
        notifyEv(pub, Remove(oldV))
        notifyEv(pub, Include(newV))
      case Reset =>
        countFalse = 0
    }
  }
}

case class IndexBy[T, Repr <: Traversable[T] with TraversableLike[T, Repr], K](base: Exp[Repr], f: Exp[T => K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, Repr]) extends Arity2OpExp[Repr,
  T => K, immutable.Map[K, Repr], IndexBy[T, Repr, K]](base, f) {
  override def interpret() = CollectionUtils.groupBy(base.interpret())(f.interpret())(cbf)
    //base.interpret() groupBy f.interpret()
  override def copy(base: Exp[Repr], f: Exp[T => K]) = IndexBy(base, f)
}
case class GroupBy[T, Repr <: Traversable[T] with TraversableLike[T, Repr], K](base: Exp[Repr], f: Exp[T => K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, Repr]) extends Arity2OpExp[Repr,
  T => K, immutable.Map[K, Repr], GroupBy[T, Repr, K]](base, f) {
  override def interpret() = base.interpret().groupBy(f.interpret())
    //base.interpret() groupBy f.interpret()
  override def copy(base: Exp[Repr], f: Exp[T => K]) = GroupBy(base, f)
}

case class Join[T, Repr <: TraversableLike[T, Repr], S, TKey, TResult, That](colouter: Exp[Repr],
                                                                             colinner: Exp[Traversable[S]],
                                                                             outerKeySelector: Fun[T, TKey],
                                                                             innerKeySelector: Fun[S, TKey],
                                                                             resultSelector: Fun[(T, S), TResult])
                                                                            (implicit cbf: CanBuildFrom[Repr, TResult, That]) extends
Arity5Op[Exp[Repr],
  Exp[Traversable[S]],
  Fun[T, TKey], Fun[S, TKey], Fun[(T, S), TResult],
  That, Join[T, Repr, S, TKey, TResult, That]](colouter, colinner, outerKeySelector, innerKeySelector, resultSelector) {
  override def copy(colouter: Exp[Repr],
                    colinner: Exp[Traversable[S]],
                    outerKeySelector: Fun[T, TKey],
                    innerKeySelector: Fun[S, TKey],
                    resultSelector: Fun[(T, S), TResult]) = Join(colouter, colinner, outerKeySelector, innerKeySelector, resultSelector)

  override def interpret() = {
    // naive hash join algorithm
    val ci: Traversable[S] = colinner.interpret()
    val co: Repr = colouter.interpret()
    val builder = cbf(co)
    // In databases, we build the temporary index on the smaller relation, so that the index fits more easily in
    // memory. This concern seems not directly relevant here; what matters here is only whether insertions or lookups in a
    // hash-map are more expensive. OTOH, it is probably important that the temporary index fits at least in the L2 cache,
    // so we should index again on the smaller relation!
    //if (ci.size > co.size) {
      val map = ci.groupBy(innerKeySelector.interpret()) //Cost O(|ci|) hash-map insertions
      for (c <- co; d <- map(outerKeySelector.interpret()(c))) //Cost O(|co|) hash-map lookups
        builder += resultSelector.interpret()(c, d)
    //XXX: this is non-order-preserving, and might be suboptimal.
    /*} else {
      val map = co.groupBy(outerKeySelector.interpret())
      for (c <- ci; d <- map(innerKeySelector.interpret()(c)))
        builder += resultSelector.interpret()(d, c)
    }*/
    builder.result()
  }
}

import collection.immutable.Seq
object ExpSeq {
  def apply[T](children: Traversable[Exp[T]]): Exp[Seq[T]] = ExpSeq(children.toList)
}

case class ExpSeq[T](children: Seq[Exp[T]]) extends Exp[Seq[T]] {
  override def nodeArity = children.size
  override protected def checkedGenericConstructor: collection.Seq[Exp[_]] => Exp[Seq[T]] = v => ExpSeq((v.toList.asInstanceOf[Seq[Exp[T]]]))
  override def interpret() = children.map(_.interpret())
}

case class Contains[T](set: Exp[Set[T]], v: Exp[T]) extends Arity2OpExp[Set[T], T, Boolean, Contains[T]](set, v) {
  def interpret() = set.interpret().contains(v.interpret())
  def copy(set: Exp[Set[T]], v: Exp[T]) = Contains(set: Exp[Set[T]], v: Exp[T])
}
