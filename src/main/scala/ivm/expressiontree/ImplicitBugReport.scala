package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 23/8/2011
 */

object ImplicitBugReport {
  trait Exp[+T] {
    def interpret: T
    //For Exp[Traversable[T]], we would like to have some extra methods available.
  }

  case class Const[T](t: T) extends Exp[T] {
    def interpret = t
  }

  class ExpWrapper[+T](t: Exp[T]) extends Exp[T] {
    def interpret = t.interpret
  }

  trait CanBuildExp[-Elem, +To] extends (Exp[Elem] => To) {
    def apply(e: Exp[Elem]): To
    //XXX: This naming is probably better - maybe instances would then work as implicit conversions themselves!
  }

  trait TraversableExp[T, ExpT <: Exp[T]] extends Exp[Traversable[T]] {
    //extra methods, which make sense only in Exp[Traversable[T]]
  }

  case class TraversableExpWrap[T, ExpT <: Exp[T]](t: Exp[Traversable[T]])(implicit val cbeT: CanBuildExp[T, ExpT]) extends
      ExpWrapper[Traversable[T]](t) with TraversableExp[T, ExpT]

  implicit def canBuildExp[T] = new CanBuildExp[T, Exp[T]] {
    def apply(e: Exp[T]) = e
  }

  /* implicit */ def toExpTempl[T, That](t: T)(implicit c: CanBuildExp[T, That]): That = c apply Const(t)

  implicit def canBuildExpTrav[T, ExpT <: Exp[T]](implicit c: CanBuildExp[T, ExpT]): CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] =
    new CanBuildExp[Traversable[T], TraversableExp[T, ExpT]] {
      def apply(e: Exp[Traversable[T]]) = TraversableExpWrap(e)(c)
    }

  //We also want to define implicit conversions:
  //implicit def toExp[T](t: T): Exp[T] = toExpTempl(t)
  //implicit def toTraversableExp[T, ExpT <: Exp[T]](t: Traversable[T])(implicit c: CanBuildExp[T, ExpT]): TraversableExp[T, ExpT] = toExpTempl(t)
  //to give the second one priority, we would define toExp in trait Base and toTraversableExp in a subclass of Base.
  //However, they are not needed here.

  def testBug() {
    println("testBug:")

    def show(name: String, v: Any) {
      print(name + ": ")
      println(v)
    }

    val a1 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - canBuildExp[Seq[Int]]: CanBuildExp[Seq[Int], Exp[Seq[Int]]] is preferred to canBuildExpTrav[Int, NumExp[Int]]: CanBuildExp[Traversable[Int], TraversableExp[Int]].
    show("a1", a1)
    //val a1a = toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav)
    //show("a1a", a1a)
    val a2 = toExpTempl(Seq(1, 2, 3, 5).toTraversable) //Doesn't work well either
    show("a2", a2)
    //val a2a = toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav) //Implicit search diverges
    val a2a = toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav(canBuildExp)) //Compiles
    //val a2a = toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav(canBuildExpTrav)) //Does not compile
    show("a2a", a2a)
    val a3: Any = toExpTempl(Seq(1, 2, 3, 5)) //Works better
    show("a3", a3)
    val a4: Exp[Seq[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well - that's equivalent to a1
    show("a4", a4)
    //val a4a: Exp[Seq[Int]] = toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav) //Does not compile.
    //show("a4a", a4a)
    val a5: Exp[Traversable[Int]] = toExpTempl(Seq(1, 2, 3, 5)) //Works well apparently.
    show("a5", a5)
    //val a6: TraversableExp[Int, Exp[Int]] = Seq(1, 2, 3, 5)
    //show("a6", a6)

    show("(like a3) toExpTempl(Seq(1, 2, 3, 5))", toExpTempl(Seq(1, 2, 3, 5)))
    //show("toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5))(canBuildExpTrav))
    show("toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav)", toExpTempl(Seq(1, 2, 3, 5).toTraversable)(canBuildExpTrav))

    println()
  }

//  def testBug() {
//    println("testBug:")
//    val a2 = toExpTempl(Seq(1, 2, 3, 5)) //Doesn't work well
//    println("a2: " + a2)
//    println("toExpTempl(Seq(1, 2, 3, 5)): " + toExpTempl(Seq(1, 2, 3, 5)))
//
//    print("Try2: toExpTempl(Seq(1, 2, 3, 5)): ")
//    println(toExpTempl(Seq(1, 2, 3, 5)))
//    println()
//  }
  def main(args: Array[String]) = testBug()
}
