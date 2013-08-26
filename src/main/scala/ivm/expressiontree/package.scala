package ivm

package object expressiontree {
  import scala.reflect.runtime.universe
  val u = universe
  type ClassTag[T] = reflect.ClassTag[T]
  val ClassTag = reflect.ClassTag
  type TypeTag[T] = u.TypeTag[T]
  def classTag[T](implicit c: ClassTag[T]) = c
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag


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
