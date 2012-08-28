package ivm
package expressiontree

case class ToString[T](e: Exp[T]) extends Arity1OpExp[T, String, ToString[T]](e) with InfixPrinting {
  def interpret() = e.interpret().toString
  def copy(e: Exp[T]) = ToString(e)
  def operator = "=="
}

object Helpers {
  def smart_toString[T](v: Exp[T]) = ToString(v)
  def smart_toString(v: Any) = v.toString

  def smart_==[T](a: Exp[T], b: Exp[T]) = Eq(a, b)
  //This method is side-effect-based, not clear how to support it.
  //def smart_synchronized[T, T0](a: T, b: =>T0) = a synchronized b
  //def smart_synchronized[T0](a: Any, b: =>T0) = a synchronized b

  //def smart_asInstanceOf[T](a: Exp[Any]) =
  //def smart_isInstanceOf[T](a: Any): Boolean = a.isInstanceOf[T] //argh...
  //def smart_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.

  //Still, I don't expect that to work. Let's use implicits instead!
  //please move soon to TypeTag, as soon as I get it.
  //Do I even need these methods to be available? What's the point of having an
  //overload for not Exp, Exp?
  def smart_isInstanceOf[T: ClassTag](a: Any): Boolean =
    implicitly[ClassTag[T]].runtimeClass.isInstance(a)
    //a.isInstanceOf[T] //argh...
  //def smart_asInstanceOf[T](a: Any): T = a.asInstanceOf[T] //no warning...weird.
  def smart_asInstanceOf[T: ClassTag](a: Any): T =
    implicitly[ClassTag[T]].runtimeClass.cast(a).asInstanceOf[T]
  def smart_asInstanceOf[T: ClassTag: TypeTag](a: Exp[Any]): Exp[T] =
    AsInstanceOf(a)
}

object MacroTest extends scala.App with tests.TestUtil {
  import Lifting._
  import Macros._
  import Helpers._

  val c1 = asExp(1)
  val c2 = asExp(2)
  /*
  //Test printf
  printf("hello %s!\n", "world")

  println("With macros: " + smart(c1.toString))
  println("With macros: " + smart(c1.toString.toString))
  println("With macros: " + smart(c1.toString + "foo"))
  println("With macros: " + smart(c1.toString()))
  println("With macros: " + smart(c1 == 2))
  showExp(smart(c1 == 2 || c2 == 1), "With macros: ")
  //Since the second member is not a tuple, this is lifted with Const instead of LiftTuple2:
  showExp((smart(c1 == 2 || c2 == 1), "With macros: "))
  //This is what we currently need!
  showExp((smart(c1 == 2 || c2 == 1), asExp("With macros: ")))
  //After all, we want to allow inside smart calls to reifying methods to have modularity, but we don't necessarily care
  //for allowing calls to non-reifying methods - am I right? Usually we in fact want to reify them. We want to reify everything inside smart, and then some (the calls to reifying methods).
  //println("With macros: " + smart(c1 synchronized 2))
  //println("With macros: " + smart(c1.synchronized[Int](2)))

  println("With macros: " + smart(c1.asInstanceOf[Int]))

  //println("With macros: " + smart(c1.isInstanceOf[Int]))
  */
  //val coll = (1 to 10).asSmart
  //val coll = List(1, 2, 3).asSmart

  val coll = asExp(List(1, 2, 3))
  val mod = asExp(2)
  val rem = asExp(1)

  val noMacro = for {
    i <- coll
    if i == rem
  } yield i

  /*
  val f0 = macroId {
    for {
      i <- coll
      //if i % mod == rem
      if i == rem
    } yield i
  }

  val f10 = macroId {
    for {
      i <- coll
    } yield i
  }

  val f15 = macroId {
    coll map (i => i)
  }

  val f15_show_2 = ctShowDebug {
    coll map (i => i) filter (i => false)
  }

  val f17 = macroId {
    coll map identity
  }

  val f20 = macroId {
    (1 to 10) map (i => i) //works
  }
  */
  /*
  val f = smart {
    for {
      i <- coll
      //if i % mod == rem
      if i == rem
    } yield i
  }
  */
  //println("With macros: " + f)
  /*val f2 = smart {
    for {
      i <- (1 to 10).asSmart
      if i % 2 == 1
    } yield i
  }*/
}

// vim: set sw=2 et:
