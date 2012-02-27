package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom

trait OptionLifting extends BaseExps {
  this: IterableOps =>
  implicit def expOption2Iterable[T](t: Exp[Option[T]]) = onExp(t)(OptionOps.OptionToIterableId, x => x: Iterable[T])

  // We would like to have this conversion available:
  //   implicit def expOption2TraversableOps[T](t: Exp[Option[T]]) = (t: Exp[Iterable[T]]): TraversableOps[T]
  // so that we can invoke Traversable methods on Exp[Option[T]]. However, we also want to have more specifics methods
  // available, for instance flatMap. We can't define two different implicit conversions providing flatMap, because they
  // would be ambiguous; giving this conversion a lower priority does not work - the lower priority one
  // provides a plausible flatMap overload, and when that fails to match it's too late for the compiler to backtrack and
  // try the other one.
  // What we need to do is to provide the different versions of flatMap on OptionOps, which is not straightforward - see
  // below.

  object OptionOps {
    /* Symbol IDs are magic constants, even if self-documenting, so they are defined here. This way, if those IDs are
     * used externally (say in Optimization), it's still possible to change them without breaking the code.
     * Symbol names might seem similar, but they are not supported by refactorings - and additionally there is no
     * builtin concept of name resolution for symbols in Scala.
     */

    val OptionMapId = 'Option$map
    val OptionFilterId = 'Option$filter
    val OptionFlatMapId = 'Option$flatMap
    val OptionToIterableId = 'Option_option2Iterable
    val SomeId = 'Some

    sealed trait FlatMappableTo[-U, +Res] {
      def flatMap[T](t: Exp[Option[T]], f: Exp[T] => Exp[U]): Exp[Res]
    }
    implicit def option[U] = new FlatMappableTo[Option[U], Option[U]] {
      override def flatMap[T](t: Exp[Option[T]], f: Exp[T] => Exp[Option[U]]): Exp[Option[U]] = onExp(t, FuncExp(f))(OptionFlatMapId, (a, b) => a flatMap b)
    }
    implicit def traversable[U] = new FlatMappableTo[Traversable[U], Iterable[U]] {
      override def flatMap[T](t: Exp[Option[T]], f: Exp[T] => Exp[Traversable[U]]) = (t: Exp[Iterable[T]]) flatMap f
    }
  }

  implicit def expToOptionOps[T](t: Exp[Option[T]]) = new OptionOps(t)
  class OptionOps[T](t: Exp[Option[T]]) {
    import OptionOps._
    def isDefined = onExp(t)('isDefined, _.isDefined)
    def get = onExp(t)('get, _.get)

    def filter(p: Exp[T] => Exp[Boolean]): Exp[Option[T]] = onExp(t, FuncExp(p))(OptionFilterId, _ filter _) //(t: Exp[Iterable[T]]) withFilter p
    //We do not lift Option.withFilter because it returns a different type; we could provide operations
    //for that type as well, but I do not see the point of doing that, especially for a side-effect-free predicate.
    def withFilter(p: Exp[T] => Exp[Boolean]) = filter(p)
    def map[U](f: Exp[T] => Exp[U]): Exp[Option[U]] = onExp(t, FuncExp(f))(OptionMapId, _ map _) //(t: Exp[Iterable[T]]) map f

    // Finally, we can't provide the two implicit conversions as overloaded version of OptionOps.flatMap - or not directly
    // with Java-style overloading, first because the two overloaded signature:
    //   def flatMap[U](f: Exp[T] => Exp[Option[U]]): Exp[Option[U]]
    //   def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]]
    // have the same erasure. The standard trick is to alter one signature with an implicit parameter:
    //   def flatMap[U](f: Exp[T] => Exp[Traversable[U]])(implicit dummy: DummyImplicit): Exp[Traversable[U]]
    // but then type inference fails for f's domain type.

    //Tillmann's suggestion was to use Haskell-style overloading by emulating type classes with implicits:
    def flatMap[U, That](f: Exp[T] => Exp[U])(implicit v: FlatMappableTo[U, That]): Exp[That] = v.flatMap(t, f)

    //Note: we do not support call-by-name parameters; therefore we currently provide only orElse, and expect the user to
    //provide a default which will never fail evalution through exceptions but only evaluate to None.
    //def getOrElse[U >: T](v: /*=> */ Exp[U]) = onExp(t, v)('Option$getOrElse, _ getOrElse _)
    def orElse[U >: T](v: /*=> */ Exp[Option[U]]) = onExp(t, v)('Option$orElse, _ orElse _)
    def getOrElse[U >: T](default: /*=> */ Exp[U]) = OptionGetOrElse(t, default) //onExp(t, v)('Option$getOrElse, _ getOrElse _)
  }

  //Support let-bindings within for-comprehensions without relying on pattern-matching.
  def Let[T](v: Exp[T]): Exp[Option[T]] = onExp(v)(OptionOps.SomeId, Some(_))

  case class ExpSeq[T](children: Exp[T]*) extends Exp[Seq[T]] {
    override def nodeArity = children.size
    override protected def checkedGenericConstructor: Seq[Exp[_]] => Exp[Seq[T]] = v => ExpSeq((v.asInstanceOf[Seq[Exp[T]]]): _*)
    override def interpret() = children.map(_.interpret())
  }

  implicit def SeqExp2ExpSeq[T](a: Seq[Exp[T]]): Exp[Seq[T]] = ExpSeq(a: _*)
}

trait ExpSugar extends ConversionDisabler2 {
  this: BaseExps =>
  //XXX: evaluate whether this interface is good.
  def NULL = toExp(null)

  implicit def toPimper[T](t: T) = new WithAsSmartCollection(t)

  implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]

  class ArrayWithAsSmartCollection[T](t: Array[T]) {
    def asSmartCollection = t: Exp[Seq[T]]
  }
  implicit def toArrayPimper[T](t: Array[T]) = new ArrayWithAsSmartCollection(t)
  //Either we use ArrayWithAsSmartCollection, or we create an implicit conversion from Exp[Array[T]] to TraverableOps[T] by adding the final cast to TraversableOps[T] here.
  //Since this is an implicit conversion, we can't just return Exp[Seq[T]] and rely on an additional implicit conversion to supply lifted collection methods.
  //implicit def expArrayToExpSeq[T](x: Exp[Array[T]]) = onExp(x)('castToSeq, x => x: Seq[T]): TraversableOps[T]

  //Simplest possible definition of Query:
  //def Query[Repr](t: Exp[Repr]): Exp[Repr] = t

  class Dummy[+T](val v: T)

  //This way, using Query verifies that it's argument is of type Exp[Traversable[T]] without needing to convert it. We could
  //maybe also restrict the result of Query so that only expResult() can be called on it.
  //
  implicit def toQuery[T](t: Exp[Traversable[T]]) = new Dummy(t)
  def Query[T](t: Dummy[Exp[Traversable[T]]]) = t.v

  //Note: all the below is probably made unnecessary once Dummy becomes covariant.

  //With all variants underneath, Scala does not manage to apply toQuery implicitly.
  //implicit def toQuery[Repr](t: Exp[Repr]): Dummy[Exp[Repr]] = new Dummy(t)
  //def Query[Repr](t: Dummy[Exp[Repr]]): Exp[Repr] = t.v

  //implicit def toQuery[Repr <: Traversable[_]](t: Exp[Repr]): Dummy[Exp[Repr]] = new Dummy(t)
  //def Query[Repr <: Traversable[_]](t: Dummy[Exp[Repr]]): Exp[Repr] = t.v

  //implicit def toQuery[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]): Dummy[Exp[Repr with Traversable[T]]] = new Dummy(t)
  //def Query[T, Repr <: Traversable[T]](t: Dummy[Exp[Repr with Traversable[T]]]): Exp[Repr with Traversable[T]] = t.v


  class Materializable[T](t: Exp[Traversable[T]]) {
    def materialize = new IncrementalResult(t)
  }
  implicit def toMaterializable[T](t: Exp[Traversable[T]]) = new Materializable(t)
}

object Lifting
  extends BaseExps with OptionLifting
  with TraversableOps with ForceOps with IterableOps with SeqOps with MapOps with SetOps with TypeFilterOps with NumOps with BaseTypesOps with ExpSugar
{
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

  // XXX: Both these and onExp should not be made available without qualification everywhere. We should just be able to
  // import them, but we should not pollute the namespace for client code.

  //Analogues of Exp.app. Given the different argument order, I needed to rename them to get a sensible name:
  def withExpFunc[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = f(t)
  //The use of _App_ and FuncExp means that t will be evaluated only once.
  def letExp[T, U](t: Exp[T])(f: Exp[T] => Exp[U]): Exp[U] = App(FuncExp(f), t)

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
  //def filterByType[S: Manifest]: Exp[PartialFunction[Any, S]] = new PartialFuncExp(x => x.ifInstanceOf[S])

  case class Elseable[T](conds: Seq[Exp[Boolean]], bodies: Seq[Exp[T]]) {
    def else_[U >: T](elseBody: Exp[U]): Exp[U] =
      (conds, bodies).zipped.foldRight(elseBody) {
        case ((cond, thenBody), curr) => IfThenElse(cond, thenBody, curr)
      }
    //This overload allows chaining if-else if. The idea comes from:
    //http://blog.razie.com/2011/08/scala-dsl-technique-if-else-constructs.html
    def else_[U >: T](branch: Elseable[U]) = Elseable(conds ++ branch.conds, bodies ++ branch.bodies)
  }
  def if_[T](cond: Exp[Boolean])(thenBody: Exp[T]) = Elseable(Seq(cond), Seq(thenBody))
}
