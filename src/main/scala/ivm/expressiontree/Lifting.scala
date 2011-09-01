package ivm.expressiontree

import collection.TraversableView

object Lifting {
  case class PairHelper[A,B](p: Exp[(A,B)]) {
    val _1 = Proj1(p)
    val _2 = Proj2(p)
  }

  implicit def toPairHelper[A,B](e: Exp[(A,B)]) : PairHelper[A,B] = PairHelper(e)

  // these functions are explicitly not implicit :)
  def liftCall[Res](callfunc: () => Res) = Call0(callfunc)
  def liftCall[A0, Res](callfunc: A0 => Res, arg0: Exp[A0]) = Call1(callfunc, arg0)
  def liftCall[A0, A1, Res](callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) = Call2(callfunc, arg0, arg1)
  def liftCall[A0, A1, A2, Res](callfunc: (A0, A1, A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) = Call3(callfunc, arg0, arg1, arg2)
  def liftCall[A0, A1, A2, A3, Res](callfunc: (A0, A1, A2, A3) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3]) = Call4(callfunc, arg0, arg1, arg2, arg3)
  def liftCall[A0, A1, A2, A3, A4, Res](callfunc: (A0, A1, A2, A3, A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) = Call5(callfunc, arg0, arg1, arg2, arg3, arg4)


  def liftFunc[S,T](f: Exp[S] => Exp[T]) : Exp[S => T] = FuncExp(f)

  implicit def liftT[T](x: T) = Const(x)
  /*implicit def liftOrd[T: Ordering](x: T) = Const(x)
  implicit def liftNum[T: Numeric](x: T) = Const(x)

  implicit def liftBool(x: Boolean) : Exp[Boolean] = Const(x)
  implicit def liftString(x: String) : Exp[String] = Const(x)*/

  implicit def liftPair[A,B](pair: (Exp[A],Exp[B])) : Exp[(A,B)] = Pair[A,B](pair._1, pair._2)

  class NumOps[T](val t: Exp[T])(implicit val isNum: Numeric[T]) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
  }

  class OrderingOps[T: Ordering](t: Exp[T]) {
    def <=(that: Exp[T]) = LEq(t, that)
  }

  class StringOps(t: Exp[String]) {
    def +(that: Exp[String]) = StringConcat(t, that)
  }

  class BooleanOps(b: Exp[Boolean]) {
    def &&(that: Exp[Boolean]) = And(b, that)
    def ||(that: Exp[Boolean]) = Or(b, that)
    def unary_! = Not(b)
  }

  implicit def expToNumOps[T: Numeric](t: Exp[T]) = new NumOps(t)
  implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
  implicit def expToStringOps(t: Exp[String]) = new StringOps(t)
  implicit def expToBooleanOps(t: Exp[Boolean]) = new BooleanOps(t)

  /*
   * In these definitions of toNumOps and toOrderingOps, implicit resolution fails because of ambiguity between liftOrd
   * and liftNum, if they are both declared - even if the ambiguity could easily be solved. The problem can be solved by
   * just having a polymorphic lift conversion. Other solutions are possible here but don't remove this ambiguity that
   * affects client code then.
   */
  implicit def toNumOps[T: Numeric](t: T) = expToNumOps(t)
  implicit def toOrderingOps[T: Ordering](t: T) = expToOrderingOps(t)
  // These definitions work even if both liftOrd and liftNum are declared.
  /*implicit def toNumOps[T: Numeric](t: T): NumOps[T] = Const(t)
  implicit def toOrderingOps[T: Ordering](t: T): OrderingOps[T] = Const(t)*/
  implicit def toStringOps(t: String) = expToStringOps(t)
  implicit def toBooleanOps(t: Boolean) = expToBooleanOps(t)

  // Some experimental implicit conversions.
  // With the current Scala compiler, given (f_ )(x), the compiler will try to use implicit conversion on (f _), because
  // that code is equivalent to (f _).apply(x), and the compiler applies conversion to the target of a method invocation.
  // However, given f(x), since f is just a method name and not the target of a method invocation, the compiler will not
  // apply implicit conversions, including the ones below. This is highly irregular, and hopefully could be solved
  // through a compiler plugin.
  object FunctionLifter {
    //Such an implicit conversion makes no sense - it might be needed if the function call, instead of its result, is
    //to be present in the expression tree, but the compiler will not insert this call, but rather a Const conversion on
    //the result. Should Const take its argument by-name? It can be argued that it should instead take its argument
    //by-value.

    //implicit def liftCall0[Res](f: () => Res) = Call0(f)

    implicit def liftCall1[A0, Res](f: A0 => Res):
      Exp[A0] => Exp[Res] = Call1(f, _)
    implicit def liftCall2[A0, A1, Res](f: (A0, A1) => Res):
      (Exp[A0], Exp[A1]) => Exp[Res] = Call2(f, _, _)
    implicit def liftCall3[A0, A1, A2, Res](f: (A0, A1, A2) => Res):
      (Exp[A0], Exp[A1], Exp[A2]) => Exp[Res] = Call3(f, _, _, _)
    implicit def liftCall4[A0, A1, A2, A3, Res](f: (A0, A1, A2, A3) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3]) => Exp[Res] = Call4(f, _, _, _, _)
    implicit def liftCall5[A0, A1, A2, A3, A4, Res](f: (A0, A1, A2, A3, A4) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3], Exp[A4]) => Exp[Res]= Call5(f, _, _, _, _, _)
  }

  object TravLifter {
    case class Map[T, U](base: Exp[Traversable[T]], f: Exp[T => U]) extends BinaryOpExp[Traversable[T], T => U, Traversable[U]](base, f) {
      def copy(base: Exp[Traversable[T]], f: Exp[T => U]) = Map(base, f)
      override def interpret = base.interpret map f.interpret()
    }

    case class FlatMap[T, U](base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) extends BinaryOpExp[Traversable[T], T => Traversable[U], Traversable[U]](base, f) {
      def copy(base: Exp[Traversable[T]], f: Exp[T => Traversable[U]]) = FlatMap(base, f)
      override def interpret = base.interpret flatMap f.interpret()
    }

    /*case class WithFilter[T](base: Exp[Traversable[T]], f: Exp[T => Boolean]) extends Exp[Traversable[T]] {
      //XXX: Again the same problem with filtering - we cannot call withFilter.
      override def interpret = base.interpret.view filter f.interpret
    }*/
    case class Union[T](lhs: Exp[Traversable[T]], rhs: Exp[Traversable[T]]) extends BinaryOpSymmExp[Traversable[T], Traversable[T]](lhs, rhs) {
      def copy(base: Exp[Traversable[T]], that: Exp[Traversable[T]]) = Union(base, that)
      override def interpret = lhs.interpret ++ rhs.interpret
    }

    case class View[T](base: Exp[Traversable[T]]) extends UnaryOpExp[Traversable[T], TraversableView[T, Traversable[T]]](base) {
      override def copy(base: Exp[Traversable[T]]) = View(base)
      override def interpret = base.interpret.view
    }

    case class WithFilter2[T](base: Exp[TraversableView[T, Traversable[T]]], f: Exp[T => Boolean]) extends BinaryOpExp[TraversableView[T, Traversable[T]], T => Boolean, Traversable[T]](base, f) {
      override def copy(base: Exp[TraversableView[T, Traversable[T]]], f: Exp[T => Boolean]) = WithFilter2(base, f)
      override def interpret = base.interpret filter f.interpret()
    }

    class TraversableOps[T](val t: Exp[Traversable[T]]) /*extends Exp[Traversable[T]]*/ {
      def map[U](f: Exp[T] => Exp[U]): Exp[Traversable[U]] =
        Map(this.t, FuncExp(f))

      def flatMap[U](f: Exp[T] => Exp[Traversable[U]]): Exp[Traversable[U]] =
        FlatMap(this.t, FuncExp(f))

      def withFilter(f: Exp[T] => Exp[Boolean]): Exp[Traversable[T]] =
        WithFilter2(View(this.t), FuncExp(f))

      def union[U >: T](that: Exp[Traversable[U]]): Exp[Traversable[U]] =
        Union(this.t, that)
    }
  }
}
