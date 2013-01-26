package ivm

package object expressiontree {
  import scala.reflect.runtime.universe
  val u = universe
  type ClassTag[T] = reflect.ClassTag[T]
  val ClassTag = reflect.ClassTag
  type TypeTag[T] = u.TypeTag[T]
  def classTag[T](implicit c: ClassTag[T]) = c
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag


  implicit def toAtomImplicit[T](d: Def[T]): Exp[T] = BaseLangImpl toAtom d
  implicit def toFunSymImplicit[S, T](f: Fun[S, T]): FunSym[S, T] = BaseLangImpl toFunSym f

  type Var = TypedVar[_]
  //With 2.10, these must be defined together.
  object Var {
    def apply(id: Int) = TypedVar[Nothing](id)
    def unapply(x: Var): Option[Int] = TypedVar.unapply(x)
  }
  //case class Var(override val id: Int) extends TypedVar[Nothing](id)

  //From http://stackoverflow.com/questions/6834000/scala-partialfunctions-from-concrete-ones, altered by avoiding
  //refinement types.
  implicit def funcAsPartial[A, B](f: A => B) = new PartialFunctionOps(f)

  class PartialFunctionOps[A, B](f: A => B) {
    /** only use if `f` is defined everywhere */
    def asPartial = new PartialFunction[A, B] {
      def isDefinedAt(a: A) = true
      def apply(a: A) = f(a)
    }
  }
}
