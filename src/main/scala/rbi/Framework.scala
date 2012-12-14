package rbi

import ivm.expressiontree.{GlobalFuncCall1, Const, FunSym, Sym, Fun, Def, Exp, TypeTag, ClassTag}
import collection.mutable

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */
trait LiftableApi[T] {
  def classTag: ClassTag[T]
  def typeTag: TypeTag[T]
}

trait BaseLangIntf {
  type Rep[+T]
  type Liftable[T] <: LiftableApi[T]
}
object BaseLangImpl {
  //Fast version.
  def toAtom[T](d: Def[T]): Exp[T] = d match {
    case df: Fun[s, t] => toFunSym[s, t](df).asInstanceOf[Exp[T]]
    case _ => Sym(d)
  }

  def toFunSym[S, T](d: Fun[S, T]): FunSym[S, T] = FunSym(d)
}
trait BaseLangImpl {
  import BaseLangImpl._
  type Rep[+T] = Exp[T]
}
trait ConversionDisablerLangIntf extends BaseLangIntf {
  implicit def noToExpForUnit: Liftable[Unit] = ???
  implicit def noToExpForUnitConflict: Liftable[Unit] = ???
  implicit def noPureForExp[T](t: Rep[T]): Rep[Rep[T]]
}

trait ConversionDisablerCollectionsLangIntf extends BaseLangIntf {
  implicit def noConstForMutableColl[T](t: mutable.Traversable[T]): Rep[mutable.Traversable[T]]
}
trait ConversionDisabler extends ConversionDisablerLangIntf with ConversionDisablerCollectionsLangIntf

//Implementation details, not in the language interface.
trait ConversionHelpers extends BaseLangIntf {
  def pureExpl[T: Liftable](t: T): Exp[T] = Const(t)(implicitly[Liftable[T]].classTag, implicitly[Liftable[T]].typeTag)
  //Use something derived from the above to lift other implicit conversions.
  def convLift[T, U](t: Exp[T], name: Symbol, prefix: String)(implicit conv: T => U): Exp[U] =
    new GlobalFuncCall1(name, prefix, conv, t)
  def convFromBase[T <% U, U: Liftable](t: T): Exp[U] = pureExpl(t: U)
}

trait LiftingConvsLangIntf extends ConversionDisablerLangIntf with ConversionDisablerCollectionsLangIntf {
  //Add a typeclass constraint, instead of ugly tricks to disable the conversion for specific classes.
  //implicit def pure[T](t: T): Rep[T]
  implicit def pure[T: Liftable](t: T): Rep[T]
  def asExp[T](t: Rep[T]): Rep[T]

  abstract class WithAsSquopt[T](t: T) {
    def asSquopt(implicit conv: T => Rep[T]): Rep[T]
  }
}

//Conversions which should have lower priority than pure.
trait ExtraConversions extends ConversionHelpers {
  //  implicit def int2ExpDouble(t: Int) = convFromBase[Int, Double](t)
  //  implicit def int2ExpLong(t: Int) = convFromBase[Int, Long](t)
  //  implicit def int2ExpFloat(t: Int) = convFromBase[Int, Float](t)
}

trait LiftingConvs extends BaseLangImpl with ConversionDisabler with ExtraConversions with LiftingConvsLangIntf {
  //The following variant would avoid ugliness like:
  //implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]
  //but it does not work (bug https://issues.scala-lang.org/browse/SI-3346).
  //implicit def pure[T, U <% T](t: U): Exp[T] = Const(t)
  //So let's keep it simple.
  implicit def pure[T: Liftable](t: T): Exp[T] = pureExpl(t)
  //Failed experiment - allow ignoring calls to pure when they become unneeded. Just a hack.
  //def pure[T: ClassTag: TypeTag](t: Exp[T]): Exp[T] = t
  //Of course, this fails: overloading conversions makes them unavailable as
  //implicit views, so asSquopt stops working.

  //Used to force insertion of the appropriate implicit conversion - unlike ascriptions, one needn't write out the type
  //parameter of Exp here.
  def asExp[T](t: Exp[T]) = t

  class WithAsSquopt[T](t: T) extends super.WithAsSquopt(t) {
    override def asSquopt(implicit conv: T => Exp[T]) = conv(t)
  }
}
