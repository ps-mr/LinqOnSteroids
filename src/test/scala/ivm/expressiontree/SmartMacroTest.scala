package ivm
package expressiontree

object SmartMacroTest extends scala.App with tests.TestUtil {
  import Lifting._
  import Macros._
  import Helpers._

  val c1 = asExp(1)
  val c2 = asExp(2)

  println("With macros: " + smart(c1.asInstanceOf[Int]))
  println("With macros: " + smart(c1.isInstanceOf[Int]))

  println("With macros: " + smart(c1 synchronized 2))
  println("With macros: " + smart(c1.synchronized[Int](2)))

  println("With macros: " + smart(c1.toString))
  println("With macros: " + smart(c1.toString.toString))
  println("With macros: " + smart(c1.toString + "foo"))
  println("With macros: " + smart(c1.toString()))
  println("With macros: " + smart(c1 == 2))
  showExp(smart(c1 == 2 || c2 == 1), "With macros: ")
  //Since the second member is not a tuple, this is lifted with Const instead of LiftTuple2:
  showExp((smart(c1 == 2 || c2 == 1), "With macros: "))
  //This is what we currently need!
  showExp((smart(c1 == 2 || c2 == 1), asExp("With macros: ")), "Pair")
  //But we can also write:
  showExp(smart((c1 == 2 || c2 == 1), "With macros: "), "Pair")
  showExp(smart(((c1 == 2 || c2 == 1), "With macros: ")), "Pair")
  //After all, we want to allow inside smart calls to reifying methods to have modularity, but we don't necessarily care
  //for allowing calls to non-reifying methods - am I right? Usually we in fact want to reify them. We want to reify everything inside smart, and then some (the calls to reifying methods).

  //val coll = (1 to 10).asSmart
  //val coll = List(1, 2, 3).asSmart
  val coll = asExp(List(1, 2, 3))
  val mod = asExp(2)
  val rem = asExp(1)

  {
    val f = smart {
      for {
        i <- coll
      } yield i
    }

    println("With macros: " + f)
    println("With macros, after code: " + (Compile toCode f))
  }
  {
    val f = smart {
      for {
        i <- coll
        if i % mod == rem
        if i == rem
      } yield i
    }

    println("With macros: " + f)
    println("With macros, after code: " + (Compile toCode f))
  }
  {
    val f = smart {
      for {
        i <- (1 to 10).asSmart
        if i % 2 == 1
      } yield i
    }
    println("With macros: " + f)
    println("With macros, after code: " + (Compile toCode f))
  }
  {
    val f = smart {
      for {
        i <- (1 to 10).asSmart
        if i % 2 == 1
      } yield (i, i + 1)
    }
    println("With macros: " + f)
    println("With macros, after code: " + (Compile toCode f))
  }
  {
    val f = smart {
      for {
        i <- (1 to 10).asSmart
        if i % 2 == 1
      } yield (i, (i + 1, i + 2))
    }
    println("With macros: " + f)
    println("With macros, after code: " + (Compile toCode f))
  }
  //*/
}

// vim: set sw=2 et:
