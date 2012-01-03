package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom

trait OptionLifting extends BaseExps {
  implicit def expToOptionOps[T](t: Exp[Option[T]]) = new OptionOps(t)
  class OptionOps[T](t: Exp[Option[T]]) {
    def isDefined = onExp(t)('isDefined, _.isDefined)
    def get = onExp(t)('get, _.get)
    //We do not use Option.withFilter because it returns a different type; we could provide operations
    //for that type as well, but I do not see the point of doing that, especially for a side-effect-free predicate.
    def withFilter(p: Exp[T] => Exp[Boolean]) = onExp(t, FuncExp(p))('filter, _ filter _)
    def map[U](f: Exp[T] => Exp[U]) = onExp(t, FuncExp(f))('map, _ map _)
    def flatMap[U](f: Exp[T] => Exp[Traversable[U]]) = onExp(t, FuncExp(f))('flatMap, (a, b) => (a: Iterable[T]) flatMap b)

    //Note: we do not support call-by-name parameters; therefore we currently provide only orElse, and expect the user to
    //provide a default which will never fail evalution through exceptions but only evaluate to None.
    //def getOrElse[U >: T](v: /*=> */ Exp[U]) = onExp(t, v)('Option$getOrElse, _ getOrElse _)
    def orElse[U >: T](v: /*=> */ Exp[Option[U]]) = onExp(t, v)('Option$orElse, _ orElse _)
  }
  implicit def expOption2Iterable[T](t: Exp[Option[T]]) = onExp(t)('Option_option2Iterable, x => x: Iterable[T])

  //Support let-bindings within for-comprehensions without relying on pattern-matching.
  def Let[T](v: Exp[T]): Exp[Option[T]] = onExp(v)('Some, Some(_))

  case class ExpSeq[T](children: Exp[T]*) extends Exp[Seq[T]] {
    override def nodeArity = children.size
    override protected def checkedGenericConstructor: Seq[Exp[_]] => Exp[Seq[T]] = v => ExpSeq((v.asInstanceOf[Seq[Exp[T]]]): _*)
    override def interpret() = children.map(_.interpret())
  }
}

trait ExpSugar {
  this: BaseExps =>
  //XXX: evaluate whether this interface is good.
  def NULL = toExp(null)

  class Pimper[T](t: T) {
    def asSmartCollection = asExp(t)
  }
  implicit def toPimper[T](t: T) = new Pimper(t)

  class ArrayPimper[T](t: Array[T]) {
    def asSmartCollection = asExp(t: Seq[T])
  }
  implicit def toArrayPimper[T](t: Array[T]) = new ArrayPimper(t)
  //Either we use ArrayPimper, or we create an implicit conversion from Exp[Array[T]] to TraverableOps[T] by adding the final cast to TraversableOps[T] here.
  //Since this is an implicit conversion, we can't just return Exp[Seq[T]] and rely on an additional implicit conversion to supply lifted collection methods.
  //implicit def expArrayToExpSeq[T](x: Exp[Array[T]]) = onExp(x)('castToSeq, x => x: Seq[T]): TraversableOps[T]

  class Materializable[T](t: Exp[Traversable[T]]) {
    def materialize = new IncrementalResult(t)
  }
  implicit def toMaterializable[T](t: Exp[Traversable[T]]) = new Materializable(t)
}

object Lifting
  extends BaseExps with OptionLifting
  with TraversableOps with MapOps with SetOps with TypeFilterOps with NumOps with BaseTypesOps with ExpSugar
{
  def liftFunc[S, T](f: Exp[S] => Exp[T]): Exp[S => T] = FuncExp(f)

  override def groupBySelImpl[T, Repr <: Traversable[T] with
    TraversableLike[T, Repr], K, Rest, That <: Traversable[Rest]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                             g: Exp[T] => Exp[Rest])(
    implicit c: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]] =
  {
    implicit def expToTraversableLikeOps[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](v: Exp[Repr with Traversable[T]]) =
      new TraversableLikeOps[T, Traversable, Repr] {val t = v}

    //val tmp: Exp[Map[K, Repr]] = t.groupBy(f) //can't write this, because we have no lifting for TraversableLike
    //val tmp: Exp[Map[K, Repr]] = GroupBy(t, FuncExp(f))
    val tmp: Exp[Map[K, Repr]] = expToTraversableLikeOps(t).groupBy(f)
    //tmp.map(v => (v._1, MapOp(v._2, FuncExp(g)))) //This uses MapOp directly, but map could return other nodes
    tmp.map(v => (v._1, expToTraversableLikeOps(v._2).map(g)(c)))
  }

  //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
  def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)
  //The use of _App_ and FuncExp means that t will be evaluated only once.
  def letExp[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = App(FuncExp(f), t)

  implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]

  // Some experimental implicit conversions.
  // With the current Scala compiler, given (f_ )(x), the compiler will try to use implicit conversion on (f _), because
  // that code is equivalent to (f _).apply(x), and the compiler applies conversion to the target of a method invocation.
  // However, given f(x), since f is just a method name and not the target of a method invocation, the compiler will not
  // apply implicit conversions, including the ones below. This is highly irregular, and hopefully could be solved
  // through a compiler plugin.
  object FunctionLifter {
    //Such an implicit conversion makes no sense - it might be needed if the function call, instead of its result, is
    //to be present in the expression tree, but the compiler will not insert this call, but rather a Const conversion on
    //the result. Should Const take its argument by-name? It can be argued that it should instead take its argument
    //by-value.

    //implicit def liftCall0[Res](f: () => Res) = Call0(f)

    implicit def liftCall1[A0, Res](id: Symbol, f: A0 => Res):
      Exp[A0] => Exp[Res] = new Call1(id,f, _)
    implicit def liftCall2[A0, A1, Res](id: Symbol, f: (A0, A1) => Res):
      (Exp[A0], Exp[A1]) => Exp[Res] = new Call2(id,f, _, _)
    implicit def liftCall3[A0, A1, A2, Res](id: Symbol, f: (A0, A1, A2) => Res):
      (Exp[A0], Exp[A1], Exp[A2]) => Exp[Res] = new Call3(id,f, _, _, _)
    implicit def liftCall4[A0, A1, A2, A3, Res](id: Symbol, f: (A0, A1, A2, A3) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3]) => Exp[Res] = new Call4(id,f, _, _, _, _)
    implicit def liftCall5[A0, A1, A2, A3, A4, Res](id: Symbol, f: (A0, A1, A2, A3, A4) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3], Exp[A4]) => Exp[Res]= new Call5(id,f, _, _, _, _, _)
  }

  // maybe this is not the best place to define this function
  def filterByType[S: Manifest]: Exp[PartialFunction[Any,S]] = new PartialFuncExp( (x) => x.ifInstanceOf[S])
}
