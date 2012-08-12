package ivm.expressiontree

import collection.generic.{CanBuildFrom}
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
                            (implicit /*protected[this]*/ val c: CanBuildFrom[Repr, U, That]) extends Arity2Op[Exp[Repr], Fun[T, Traversable[U]], That, FlatMap[T, Repr, U, That]](base, f) with InfixPrinting {
  override def interpret() = base.interpret() flatMap f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Traversable[U]]) = FlatMap[T, Repr, U, That](base, f)
  def operator = "flatMap"
}

case class MapNode[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 U, That <: Traversable[U] with TraversableLike[U, That]](base: Exp[Repr with Traversable[T]], f: Fun[T, U])
                          (implicit /*protected[this] */val c: CanBuildFrom[Repr with Traversable[T], U, That with Traversable[U]]) extends Arity2Op[Exp[Repr], Fun[T, U], That, MapNode[T, Repr, U, That]](base, f) with InfixPrinting {
  override def interpret() = base.interpret() map f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, U]) = MapNode[T, Repr, U, That](base, f)
  def operator = "map"
}

case class Filter[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with Traversable[T]],
                                                      f: Fun[T, Boolean]) extends Arity2Op[Exp[Repr], Fun[T, Boolean], Repr, Filter[T, Repr]](base, f) with InfixPrinting {
  override def interpret() = base.interpret() filter f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Boolean]) = Filter(base, f)
  def operator = "filter"
}

case class WithFilter[T, Repr <: TraversableLike[T, Repr]](base: Exp[Repr],
                                                      f: Fun[T, Boolean])
                                                      extends Arity2Op[Exp[Repr], Fun[T, Boolean], TraversableView[T, Repr], WithFilter[T, Repr]](base, f) with InfixPrinting {
  override def interpret() = base.interpret().view filter f.interpret()
  override def copy(base: Exp[Repr], f: Fun[T, Boolean]) = WithFilter[T, Repr](base, f)
  def operator = "withFilter"
}

case class View[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with Traversable[T] with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, TraversableView[T, Repr], View[T, Repr]](base) with InfixPrinting {
  override def interpret() = base.interpret().view
  override def copy(base: Exp[Repr]) = View(base)
  def operator = "view"
}

case class Force[T, Repr <: Traversable[T] with TraversableLike[T, Repr],
                 ViewColl <: TraversableViewLike[T, Repr, ViewColl] with TraversableView[T, Repr] with TraversableLike[T, ViewColl], That]
                (base: Exp[ViewColl with TraversableViewLike[T, Repr, ViewColl]])
                (implicit protected[this] val bf: CanBuildFrom[Repr, T, That]) extends Arity1OpExp[ViewColl, That, Force[T, Repr, ViewColl, That]](base) with InfixPrinting {
  override def interpret() = base.interpret().force
  override def copy(base: Exp[ViewColl]) = Force[T, Repr, ViewColl, That](base)
  def operator = "force"
}

//A bit of a hack, since it does not return the most precise type possible.
case class ForceIfPossible[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](base: Exp[Repr with TraversableLike[T, Repr]]) extends Arity1OpExp[Repr, Traversable[T], ForceIfPossible[T, Repr]](base) {
  override def interpret() = {
    Lifting.TraversableForceable.force(base.interpret())
  }
  override def copy(base: Exp[Repr]) = ForceIfPossible(base)
}

case class Union[T, Repr <: TraversableLike[T, Repr], That](base: Exp[Repr], that: Exp[Traversable[T]])
                                   (implicit protected[this] val c: CanBuildFrom[Repr, T, That]) extends Arity2OpExp[Repr, Traversable[T], That, Union[T, Repr, That]](base, that) with InfixPrinting {
  override def interpret() = base.interpret() ++ that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Union[T, Repr, That](base, that)
  def operator = "union"
}

case class Diff[T, Repr <: collection.Set[T] with SetLike[T, Repr]](base: Exp[Repr], that: Exp[Traversable[T]]) extends Arity2OpExp[Repr, Traversable[T], Repr, Diff[T, Repr]](base, that) with InfixPrinting {
  override def interpret() = base.interpret() -- that.interpret()
  override def copy(base: Exp[Repr], that: Exp[Traversable[T]]) = Diff[T, Repr](base, that)
  def operator = "--"
}

case class Size[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Int, Size[T, Repr]](t) with InfixPrinting {
  def interpret() = t.interpret().size
  def copy(t: Exp[Repr]) = Size(t)
  def operator = "size"
}

//XXX t is a duplicated field.
case class IsEmpty[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Boolean, IsEmpty[T, Repr]](t) with InfixPrinting {
  def interpret() = t.interpret().isEmpty
  def copy(t: Exp[Repr]) = IsEmpty(t)
  def operator = "isEmpty"
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
    value()
  }

  override def copy(coll: Exp[Traversable[T]]) = Forall(coll, f)

  override def cache = Some(value()) //We probably need to make the cache _field_ optional.
  override def value() = countFalse == 0 //The result is always valid here.

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
      case _ => //Should not be possible
        throw new IllegalArgumentException
    }
  }
}

case class IndexBy[T: ClassTag: TypeTag, Repr <: Traversable[T] with TraversableLike[T, Repr]: TypeTag, K](base: Exp[Repr], f: Exp[T => K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, Repr]) extends Arity2OpExp[Repr,
  T => K, immutable.Map[K, Repr], IndexBy[T, Repr, K]](base, f) with PersistValue[CanBuildFrom[Repr, T, Repr]] {
  override def interpret() = CollectionUtils.groupBy(base.interpret())(f.interpret())(cbf)
  override def copy(base: Exp[Repr], f: Exp[T => K]) = IndexBy(base, f)
  override def toCode = "ivm.expressiontree.CollectionUtils.groupBy(%s)(%s)(%s)" format (base.toCode, f.toCode, persistedValue)
  override def cTagT = implicitly
  override def tTagT = implicitly
  def valueToPersist = cbf
}
case class GroupBy[T, Repr <: Traversable[T] with TraversableLike[T, Repr], K](base: Exp[Repr], f: Exp[T => K])(implicit cbf: CanBuildFrom[Repr /* with Traversable[A]*/, T, Repr]) extends Arity2OpExp[Repr,
  T => K, immutable.Map[K, Repr], GroupBy[T, Repr, K]](base, f) with InfixPrinting {
  override def interpret() = base.interpret().groupBy(f.interpret())
  override def copy(base: Exp[Repr], f: Exp[T => K]) = GroupBy(base, f)
  def operator = "groupBy"
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

case class ExpSeq[T](children: List[Exp[T]]) extends Exp[Seq[T]] with PrefixPrinting {
  override def nodeArity = children.size
  override protected def checkedGenericConstructor(v: List[Exp[_]]): Exp[Seq[T]] = ExpSeq((v.asInstanceOf[Seq[Exp[T]]]))
  override def interpret() = children.map(_.interpret())
  def prefix = "Seq"
}

case class Contains[T](set: Exp[Set[T]], v: Exp[T]) extends Arity2OpExp[Set[T], T, Boolean, Contains[T]](set, v) with InfixPrinting {
  def interpret() = set.interpret().contains(v.interpret())
  def copy(set: Exp[Set[T]], v: Exp[T]) = Contains(set: Exp[Set[T]], v: Exp[T])
  def operator = "contains"
}
