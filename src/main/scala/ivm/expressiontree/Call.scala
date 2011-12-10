package ivm.expressiontree


/* The implicit contract: id is a string description of the
   call. In case of a field access it is the name of the field.
   In case of a method call it is the name of the parameter and the types
   of its fields in the format "foo(Int,Int)".

   Eventually the id field should be generated by the compiler to ensure consistency.

   The point of the id field is to have a non-trivial equality relation on calls, see
   also definition of equals
 */
trait Call[Res] extends Exp[Res] {
  val id: Symbol
  override def equals(other: Any) = other match {
      case that: Call[_] =>
        this.id == that.id &&
        (this.children equals that.children)
      case _ =>
        false
  }
  override def hashCode = 41 * id.hashCode + children.hashCode
  private def baseName(str: String) = str substring ((str lastIndexOf '.') + 1)
  override def toString = "%s(%s, %s)" format (baseName(getClass.getName), id, children mkString ", ")
}
