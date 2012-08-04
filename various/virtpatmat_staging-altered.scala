//Copied from ./test/files/run/virtpatmat_staging.scala in the compiler sources
//for 2.10.0-M6.
import language.implicitConversions
import language.higherKinds

trait Intf {
 type Rep[+T]
 type M[+T] = Rep[Matcher[T]]

 val __match: MatchStrategy
 abstract class MatchStrategy {
   // runs the matcher on the given input
   def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U]

   def zero: M[Nothing]
   def one[T](x: Rep[T]): M[T]
   def guard[T](cond: Rep[Boolean], thenBranch: => Rep[T]): M[T]
   def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean] // used for isDefinedAt
 }

 abstract class Matcher[+A] {
   def flatMap[B](f: Rep[A] => M[B]): M[B]
   def orElse[B >: A](alternative: => M[B]): M[B]
 }

 implicit def proxyMaybe[A](m: M[A]): Matcher[A]
 implicit def repInt(x: Int): Rep[Int]
 implicit def repBoolean(x: Boolean): Rep[Boolean]
 implicit def repString(x: String): Rep[String]

 def test = 7 match { case 5 => "foo" case _ => "bar" }
 def test2(i: Int) = i match { case j => "foo2: " + j }
 //def test3(i: Rep[Int]) = i match { case j => "foo3: " + j } //type error!
}

trait Impl extends Intf {
 type Rep[+T] = String

 object __match extends MatchStrategy {
   def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U] = ("runOrElse("+ in +", ?" + matcher("?") + ")")
   def zero: M[Nothing]                                             = "zero"
   def one[T](x: Rep[T]): M[T]                                      = "one("+x.toString+")"
   def guard[T](cond: Rep[Boolean], thenBranch: => Rep[T]): M[T]    = "guard("+cond+","+thenBranch+")"
   def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean]  = ("isSuccess("+x+", ?" + f("?") + ")")
 }

 implicit def proxyMaybe[A](m: M[A]): Matcher[A] = new Matcher[A] {
   def flatMap[B](f: Rep[A] => M[B]): M[B]                          = m + ".flatMap(? =>"+ f("?") +")"
   def orElse[B >: A](alternative: => M[B]): M[B]                   = m + ".orElse("+ alternative +")"
 }

 def repInt(x: Int): Rep[Int] = x.toString
 def repBoolean(x: Boolean): Rep[Boolean] = x.toString
 def repString(x: String): Rep[String] = x
}

object Test extends Impl with Intf with App {
  println(test)
  println(test2(2))
  //println(test3(3))
}
