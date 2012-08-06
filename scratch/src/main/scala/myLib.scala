trait Exp[+T] 
case class Const[T](e: T) extends Exp[T]
case class ToString[T](e: Exp[T]) extends Exp[String]
case class AsInstanceOf[T](e: Exp[Any], t: Manifest[T]) extends Exp[T]
case class Eq[T](a: Exp[T], b: Exp[T]) extends Exp[Boolean]
case class Or(a: Exp[Boolean], b: Exp[Boolean]) extends Exp[Boolean]

object myLib extends App {
  def dummy_toString[T](v: Exp[T]) = ToString(v)
  def dummy_toString(v: Any) = v.toString
  implicit def unit[T]: T => Exp[T] = Const(_)

  implicit class BooleanOps(a: Exp[Boolean]) {
    def ||(b: Exp[Boolean]) = Or(a, b)
  }

  def asExp[T](t: Exp[T]) = t

  def eq(a: Any, b: Any) = a == b
  def eq[T](a: Exp[T], b: T) = Eq(a, b)
  def eq[T](a: T, b: Exp[T]) = Eq(a, b)
  def eq[T](a: Exp[T], b: Exp[T]) = Eq(a, b)

  def dummy_==[T](a: Exp[T], b: Exp[T]) = Eq(a, b)
  //This method is side-effect-based, not clear how to support it.
  //def dummy_synchronized[T, T0](a: T, b: =>T0) = a synchronized b
  //def dummy_synchronized[T0](a: Any, b: =>T0) = a synchronized b

  //def dummy_asInstanceOf[T](a: Exp[Any]) = 
  //def dummy_isInstanceOf[T](a: Any): Boolean = a.isInstanceOf[T] //argh...
  //def dummy_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.

  //Still, I don't expect that to work. Let's use implicits instead!
  //please move soon to TypeTag, as soon as I get it.
  //Do I even need these methods to be available? What's the point of having an
  //overload for not Exp, Exp?
  def dummy_isInstanceOf[T: Manifest](a: Any): Boolean =
    implicitly[Manifest[T]].runtimeClass.isInstance(a)
    //a.isInstanceOf[T] //argh...
  //def dummy_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.
  def dummy_asInstanceOf[T: Manifest](a: Any): T =
    implicitly[Manifest[T]].runtimeClass.cast(a).asInstanceOf[T]
  def dummy_asInstanceOf[T: Manifest](a: Exp[Any]): Exp[T] =
    AsInstanceOf(a, implicitly[Manifest[T]])
  //def dummy_asInstanceOf[T](a: Exp[Any]) = 

  val c1 = asExp(1)
  val c2 = asExp(2)
  println(dummy_toString(1))
  println(dummy_toString(c1))
  println(eq(1, 2))
  println(eq(c1, 2))
  println(eq(1, c2))
  println(eq(c1, c2))
  import Macros._
  println("With macros: " + smart(c1.toString))
  println("With macros: " + smart(c1.toString.toString))
  println("With macros: " + smart(c1.toString + "foo"))
  println("With macros: " + smart(c1.toString()))
  println("With macros: " + smart(c1 == 2))
  println("With macros: " + smart(c1 == 2 || c2 == 1))
  //println("With macros: " + smart(c1 synchronized 2))
  //println("With macros: " + smart(c1.synchronized[Int](2)))
  println("With macros: " + smart(c1.asInstanceOf[Int]))
  //println("With macros: " + smart(c1.isInstanceOf[Int]))
}
