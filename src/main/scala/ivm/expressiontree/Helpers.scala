package ivm
package expressiontree

object Helpers {
  def smart_toString[T](v: Exp[T]) = ToString(v)
  def smart_toString(v: Any) = v.toString

  import Lifting._
  def smart_==[T](a: T, b: T) = a == b
  //Hm. I could probably obviate the need for these overloads by making the smart macro more intelligent - after all, it can invoke typeCheck.
  def smart_==[T: ClassTag: TypeTag](a: T, b: Exp[T]) = Eq(a, b)
  def smart_==[T: ClassTag: TypeTag](a: Exp[T], b: T) = Eq(a, b)
  def smart_==[T](a: Exp[T], b: Exp[T]) = Eq(a, b)
  //This method is side-effect-based, not clear how to support it.
  //def smart_synchronized[T, T0](a: T, b: =>T0) = a synchronized b
  def smart_synchronized[T0](a: AnyRef, b: =>T0) = {
    println("smart_synchronized called!")
    a synchronized b
  }

  //def smart_asInstanceOf[T](a: Exp[Any]) =
  //def smart_isInstanceOf[T](a: Any): Boolean = a.isInstanceOf[T] //argh...
  //def smart_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.

  //Still, I don't expect that to work. Let's use implicits instead!
  //please move soon to TypeTag, as soon as I get it.
  //Do I even need these methods to be available? What's the point of having an
  //overload whose type does not mention Exp?
  def smart_isInstanceOf[T: ClassTag](a: Any): Boolean =
    implicitly[ClassTag[T]].runtimeClass.isInstance(a)
    //a.isInstanceOf[T] //argh...
  //def smart_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.
  def smart_asInstanceOf[T: ClassTag: TypeTag](a: Any): T =
    implicitly[ClassTag[T]].runtimeClass.cast(a).asInstanceOf[T]
  def smart_isInstanceOf[T: ClassTag: TypeTag](a: Exp[Any]): Exp[Boolean] =
    IsInstanceOf(a)
  def smart_asInstanceOf[T: ClassTag: TypeTag](a: Exp[Any]): Exp[T] =
    AsInstanceOf(a)
}
