package ivm
package expressiontree

import Lifting._
import collection.TraversableLike

object RebuildTransform {
case class MySize[T, Repr <: Traversable[T] with TraversableLike[T, Repr]](t: Exp[Repr with Traversable[T]]) extends Arity1OpExp[Repr, Int, MySize[T, Repr]](t) with InfixPrinting {
  def interpret() = t.interpret().size
  def copy(t: Exp[Repr]) = MySize(t)
  def operator = "size"
}

  def f[T]: Exp[T] => Exp[T] = {
    /*
    case MapNode(coll, f) => coll map f
    case FlatMap(coll, f) => coll map f //coll flatMap f
    case Filter(coll, p) => coll filter p
    case Size(coll) => coll.size
    */
    case Sym(e: MapNode[t, repr, u, that]) => (e.base map e.f)(e.c)
    case Sym(e: FlatMap[t, repr, u, that]) => (e.base flatMap e.f)(e.c) //coll flatMap f
    case Sym(e: Filter[t, repr]) => e.base filter e.f
    case Sym(e: Size[t, repr]) => e.t.size
    //case e: MySize[t, repr] => MySize(e.t)
    case other => other
  }
}

object GADTMatchProblems {
  trait Exp[+T] //Triggers compile errors on most cases in the pattern match
  //sealed trait Exp[+T] //No difference with the above
  //sealed trait Exp[T] //Triggers compile errors on the SizeFixed case in the pattern match
  /*
  case class Size[T](t: Exp[T]) extends Exp[Int]
  case class Foo[T](t: Exp[T]) extends Exp[Seq[Int]]
  case class Foo2[T](t: Exp[T]) extends Exp[Seq[T]]
  case class Foo3[T, Foo](t: Exp[T]) extends Exp[Foo]

  case class SizeFixed[T, Foo <: Int](t: Exp[T]) extends Exp[Foo]
  */

  //Adding final fixes all problems here
  final case class Size[T](t: Exp[T]) extends Exp[Int]
  final case class Foo[T](t: Exp[T]) extends Exp[Seq[Int]]
  final case class Foo2[T](t: Exp[T]) extends Exp[Seq[T]]
  final case class Foo3[T, Foo](t: Exp[T]) extends Exp[Foo]

  final case class SizeFixed[T, Foo <: Int](t: Exp[T]) extends Exp[Foo]
  def f[T]: Exp[T] => Exp[T] = {
  //def f[T](expr: Exp[T]): Exp[T] = expr match {
    case e: Size[t] => Size(e.t)
    case e: Foo[t] => Foo(e.t)
    case e: Foo2[t] => Foo2(e.t)//: Exp[Seq[t]]
    case e: Foo3[t, foo] => Foo3[t, foo](e.t)
    case e: SizeFixed[t, foo] => SizeFixed[t, foo](e.t)//: Exp[foo]
    case other => other
  }
}

/*
object SI5195 {
trait F[A,B] { 
  def eval: List[A] => List[B]
  def pipe[C](f: F[B,C]): F[A,C] = Pipe(this, f) 
}
final case class Flip[A,B]() extends F[(A,B),(B,A)] {
  def eval = _ map { case (a,b) => (b,a) }
}
final case class Pipe[A,B,C](f: F[A,B], g: F[B,C]) extends F[A,C] {
  def eval = f.eval andThen g.eval
}
final case class MapF[A,B](f: A => B) extends F[A,B] { 
  def eval = _ map f
  override def pipe[C](g: F[B,C]) = g match {
    case MapF(g) => MapF(f andThen g)
    /** Commented out lines do not compile, the pattern match does not refine
     *  the type B to a (x,y) for some types, x, y */
    case g2: Flip[a,b] => MapF(f.andThen (_.swap))
    case Flip() => MapF(f andThen (_.swap))
    case _ => Pipe(this,g)
  }
}
}
object SI5195_2 {
trait Expr2[A] {
  def eval: A
}
case class Atom2[A](a: A) extends Expr2[A] {
  def eval = a
}
case class Ap2[A,B](f: Expr2[A => B], arg: Expr2[A]) extends Expr2[B] {
  def eval = f.eval(arg.eval)
}
case class Fn2[A,B](f: Expr2[A] => Expr2[B]) extends Expr2[A => B] {
  def eval = (a: A) => f(Atom2(a)).eval
}

object GADT1 {
  /** This implementation also compiles - suggested by Mark Harrah.
* Note the use of (existential) type variables in pattern. */
  def eval[A](e: Expr2[A]): A = e match {
    case Atom2(a) => a
    case Ap2(f,a) => eval(f)(eval(a))
    case f: Fn2[a,b] => ((x:a) => eval(f.f(Atom2[a](x))))
  }
  def main(args: Array[String]): Unit = {
    val x = Atom2(4)
    val factorial = Fn2 { (e: Expr2[Int]) => Atom2 ((1 to e.eval).product) }
    val e = Ap2(factorial, x)
    //val e2 = Ap2(factorial, Atom2("w00t")) // error!!
    println (e.eval)
  }
}
}
*/

// vim: set ts=8 sw=2 et:
