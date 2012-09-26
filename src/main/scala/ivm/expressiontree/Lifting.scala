package ivm.expressiontree

import collection.TraversableLike
import collection.generic.CanBuildFrom
import ivm.collections.TypeMapping

trait OptionLifting extends BaseExps {
  this: TraversableOps =>
  implicit def expOption2Iterable[T](t: Exp[Option[T]]): Exp[Iterable[T]] =
    convLift(t, OptionOps.OptionToIterableId, "Option.option2Iterable")
  //This would require extra manifests in all callers, horrible. And it requires implicit lookup during run-time
  //compilation, which for sure won't speed up the compiler.
  /*{
    override def toCode = "(%s: Iterable[%s])" format (t1.toCode, classManifest[T])
  }*/

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

    val OptionToIterableId = 'Option_option2Iterable
  }

  implicit def expToOptionOps[T](t: Exp[Option[T]]) = new OptionOps(t)
  class OptionOps[T](t: Exp[Option[T]]) {
    import OptionOps._
    def isDefined = fmap(t)('isDefined, _.isDefined)
    def get = fmap(t)('get, _.get)

    /*
    def filter(p: Exp[T] => Exp[Boolean]): Exp[Option[T]] = fmap(t, Fun(p))(OptionFilterId, _ filter _) //(t: Exp[Iterable[T]]) withFilter p
    //We do not lift Option.withFilter because it returns a different type; we could provide operations
    //for that type as well, but I do not see the point of doing that, especially for a side-effect-free predicate.
    def withFilter(p: Exp[T] => Exp[Boolean]) = filter(p)
    def map[U](f: Exp[T] => Exp[U]): Exp[Option[U]] = fmap(t, Fun(f))(OptionMapId, _ map _) //(t: Exp[Iterable[T]]) map f

    // Finally, we can't provide the two implicit conversions as overloaded version of OptionOps.flatMap - or not directly
    // with Java-style overloading, first because the two overloaded signature:
    //   def flatMap[U](f: Exp[T] => Exp[Option[U]]): Exp[Option[U]]
    //   def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]]
    // have the same erasure. The standard trick is to alter one signature with an implicit parameter:
    //   def flatMap[U](f: Exp[T] => Exp[Traversable[U]])(implicit dummy: DummyImplicit): Exp[Traversable[U]]
    // but then type inference fails for f's domain type.

    //Tillmann's suggestion was to use Haskell-style overloading by emulating type classes with implicits:
    def flatMap[U, That](f: Exp[T] => Exp[U])(implicit v: FlatMappableTo[U, That]): Exp[That] = v.flatMap(t, f)
    */
    // TODO apparently, the implicit conversions from Scala are not that powerful; for instance, Some(1) flatMap (Seq(_))
    // is not accepted. I guess I should revert this, or argue why it's better.

    def filter(p: Exp[T] => Exp[Boolean]): Exp[Iterable[T]] = (t: Exp[Iterable[T]]) filter p
    def withFilter(p: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] = (t: Exp[Iterable[T]]) withFilter p
    def map[U](f: Exp[T] => Exp[U]): Exp[Iterable[U]] = (t: Exp[Iterable[T]]) map f
    def flatMap[U, That](f: Exp[T] => Exp[Traversable[U]]) = (t: Exp[Iterable[T]]) flatMap f

    //Note: we do not support call-by-name parameters; therefore we currently provide only orElse, and expect the user to
    //provide a default which will never fail evalution through exceptions but only evaluate to None.
    //def getOrElse[U >: T](v: /*=> */ Exp[U]) = fmap(t, v)('Option$getOrElse, _ getOrElse _)
    def orElse[U >: T](v: /*=> */ Exp[Option[U]]) = fmap(t, v)('Option$orElse, _ orElse _)
    def getOrElse[U >: T](default: /*=> */ Exp[U]) = OptionGetOrElse(t, default) //fmap(t, v)('Option$getOrElse, _ getOrElse _)
  }

  //Note: even though ExpOption does not directly contain Exp nodes, it contains them indirectly, and they also need to be
  //transformed.
  case class ExpOption[T](e: Option[Exp[T]]) extends Exp[Option[T]] {
    override def children = e.toList
    override def nodeArity = if (e.nonEmpty) 1 else 0
    override protected def checkedGenericConstructor(v: List[Exp[_]]): Exp[Option[T]] = v match {
      //Note: the length of the input sequence is checked by genericConstructor and will match the current one.
      //Knowing that does not lead to simplifying this code though.
      case Nil =>
        ExpOption(None)
      case e :: Nil =>
        ExpOption(Some(e.asInstanceOf[Exp[T]]))
      case _ =>
        throw new IllegalArgumentException
    }
    override def interpret() = e.map(_.interpret())
    override def toCode = e match {
      case Some(x) => "Some(%s)" format x.toCode
      case _ => "None"
    }
  }
  implicit def OptionExp2ExpOption[T](e: Option[Exp[T]]): Exp[Option[T]] = ExpOption(e)
}

trait ExpSugarLangIntf extends ConversionDisabler2LangIntf {
  this: BaseExpsLangIntf =>
  //XXX: evaluate whether this interface is good.
  def NULL: Rep[Null]

  implicit def toPimper[T](t: T): WithAsSmartCollection[T]

  implicit def arrayToExpSeq[T: TypeTag](x: Array[T]): Rep[Seq[T]]

  abstract class ArrayWithAsSmartCollection[T: TypeTag] {
    def asSmart: Rep[Seq[T]]
  }
  implicit def toArrayPimper[T: TypeTag](t: Array[T]): ArrayWithAsSmartCollection[T]
  //Simplest possible definition of Query:
  //def Query[Repr](t: Exp[Repr]): Exp[Repr] = t

  abstract class UnconvertedExp[+T]
  type UnconvertedExpImpl[+T] <: UnconvertedExp[T]

  //This way, using Query verifies that it's argument is of type Exp[Traversable[T]] without needing to convert it. We could
  //maybe also restrict the result of Query so that only value() can be called on it.
  //
  //implicit def toQuery[T](t: Exp[Traversable[T]]) = new UnconvertedExp(t)
  //We can use this stronger version:
  implicit def toQuery[T, Repr <: Traversable[T]](t: Rep[Repr with Traversable[T]]): UnconvertedExpImpl[Rep[Repr with Traversable[T]]]
  //We also need this version:
  implicit def toTypeIndexDummy[C[X] <: TraversableLike[X, C[X]], D[+_], Base](t: Rep[TypeMapping[C, D, Base]]): UnconvertedExpImpl[Rep[TypeMapping[C, D, Base]]]
  def Query[T, Repr <: Traversable[T]](t: UnconvertedExpImpl[Rep[Repr with Traversable[T]]]): Rep[Repr with Traversable[T]]
}

trait ExpSugar extends ConversionDisabler2 with ExpSugarLangIntf {
  this: BaseExps with TraversableOps with BaseLang =>
  //XXX: evaluate whether this interface is good.
  def NULL = pure(null)

  implicit def toPimper[T](t: T) = new WithAsSmartCollection(t)

  implicit def arrayToExpSeq[T: TypeTag](x: Array[T]): Exp[Seq[T]] = x: Seq[T]

  class ArrayWithAsSmartCollection[T: TypeTag](t: Array[T]) extends super.ArrayWithAsSmartCollection[T] {
    def asSmart = t: Exp[Seq[T]]
  }
  implicit def toArrayPimper[T: TypeTag](t: Array[T]) = new ArrayWithAsSmartCollection(t)
  //Either we use ArrayWithAsSmartCollection, or we create an implicit conversion from Exp[Array[T]] to TraverableOps[T] by adding the final cast to TraversableOps[T] here.
  //Since this is an implicit conversion, we can't just return Exp[Seq[T]] and rely on an additional implicit conversion to supply lifted collection methods.
  //implicit def expArrayToExpSeq[T](x: Exp[Array[T]]) = fmap(x)('castToSeq, x => x: Seq[T]): TraversableOps[T]

  //Simplest possible definition of Query:
  //def Query[Repr](t: Exp[Repr]): Exp[Repr] = t

  class UnconvertedExp[+T](val v: T) extends super.UnconvertedExp[T]
  type UnconvertedExpImpl[+T] = UnconvertedExp[T]

  //This way, using Query verifies that it's argument is of type Exp[Traversable[T]] without needing to convert it. We could
  //maybe also restrict the result of Query so that only value() can be called on it.
  //
  //implicit def toQuery[T](t: Exp[Traversable[T]]) = new UnconvertedExp(t)
  //We can use this stronger version:
  override implicit def toQuery[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]): UnconvertedExp[Exp[Repr with Traversable[T]]] = new UnconvertedExp(t)
  //We also need this version:
  override implicit def toTypeIndexDummy[C[X] <: TraversableLike[X, C[X]], D[+_], Base](t: Exp[TypeMapping[C, D, Base]]) = new UnconvertedExp(t)
  //def Query[T](t: UnconvertedExp[Exp[Traversable[T]]]) = t.v
  def Query[T, Repr <: Traversable[T]](t: UnconvertedExp[Exp[Repr with Traversable[T]]]): Exp[Repr with Traversable[T]] = t.v

  //This does not work, because it does not constrain type inference enough.
  //implicit def toUnconvertedExp[Repr](t: Exp[Repr]): UnconvertedExp[Exp[Repr]] = new UnconvertedExp(t)

  //Note: all the below is probably made unnecessary once UnconvertedExp becomes covariant.

  //With all variants underneath, Scala does not manage to apply toQuery implicitly.
  //implicit def toQuery[Repr](t: Exp[Repr]): UnconvertedExp[Exp[Repr]] = new UnconvertedExp(t)
  //def Query[Repr](t: UnconvertedExp[Exp[Repr]]): Exp[Repr] = t.v

  //implicit def toQuery[Repr <: Traversable[_]](t: Exp[Repr]): UnconvertedExp[Exp[Repr]] = new UnconvertedExp(t)
  //def Query[Repr <: Traversable[_]](t: UnconvertedExp[Exp[Repr]]): Exp[Repr] = t.v

  //implicit def toQuery[T, Repr <: Traversable[T]](t: Exp[Repr with Traversable[T]]): UnconvertedExp[Exp[Repr with Traversable[T]]] = new UnconvertedExp(t)
  //def Query[T, Repr <: Traversable[T]](t: UnconvertedExp[Exp[Repr with Traversable[T]]]): Exp[Repr with Traversable[T]] = t.v


  class Materializable[T](t: Exp[Traversable[T]]) {
    def materialize = new IncrementalResult(t)
  }
  implicit def toMaterializable[T](t: Exp[Traversable[T]]) = new Materializable(t)
}

trait MiscLiftingLangIntf extends LangIntf {
  /** Support let-bindings within for-comprehensions without relying on pattern-matching. */
  def Let[T](e: Rep[T]): Rep[Seq[T]]

  def groupBySelImpl[T: ClassTag: TypeTag, Repr <: Traversable[T] with
    TraversableLike[T, Repr]: TypeTag, K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](t: Rep[Repr], f: Rep[T] => Rep[K],
                                             g: Rep[T] => Rep[Rest])(
    implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Rep[Map[K, That]]
}

trait MiscLifting extends BaseExps with BaseTypesOps with TraversableOps with SeqOps with MiscLiftingLangIntf {
  def Let[T](e: Exp[T]): Exp[Seq[T]] = Seq(e)
  //def Let[T](e: Exp[T]): Exp[Option[T]] = Some(e)

  override def groupBySelImpl[T: ClassTag: TypeTag, Repr <: Traversable[T] with
    TraversableLike[T, Repr]: TypeTag, K, Rest, That <: Traversable[Rest] with TraversableLike[Rest, That]](t: Exp[Repr], f: Exp[T] => Exp[K],
                                             g: Exp[T] => Exp[Rest])(
    implicit cbf: CanBuildFrom[Repr, T, Repr], cbf2: CanBuildFrom[Repr, Rest, That]): Exp[Map[K, That]] =
  {
    t.indexBy(f).map(v => (v._1, (v._2 map g)(cbf2)))
  }

  // maybe this is not the best place to define this function
  //def filterByType[S: Manifest]: Exp[PartialFunction[Any, S]] = new PartialFuncExp(x => x.ifInstanceOf[S])
}

trait LiftingLangIntf
  extends LangIntf with ConversionDisablerLangIntf with LiftingConvsLangIntf with ConversionDisabler2LangIntf with
          FunctionOpsLangIntf with NumOpsLangIntf with BaseExpsLangIntf with BaseTypesOpsLangIntf with MiscLiftingLangIntf with IfElseLangIntf
          with TraversableOpsLangIntf
object Lifting extends LiftingLangIntf with LiftingTrait
trait LiftingTrait
  extends BaseExps with OptionLifting
  with TraversableOps with ForceOps with IterableOps with SeqOps with MapOps with SetOps with TypeFilterOps
  with NumOps with BaseTypesOps with JavaLibOps with ScalaLibOps with ExpSugar with NumConvOps with MiscLifting with IfElse

