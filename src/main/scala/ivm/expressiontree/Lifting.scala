package ivm.expressiontree


object Lifting {

  //In Haskell:
  //instance Numeric a => Summable a (Plus a) where...
  implicit def sumNum[T](implicit num: Numeric[T]) = new Summable[T, Plus[T]] {
    def plus(a: T, b: T) = num.plus(a, b)
    def plusNode(a: Exp[T], b: Exp[T]): Plus[T] = Plus(a, b)(this)
  }

  implicit val concatString = new Summable[String, StringConcat] {
    def plus(a: String, b: String) = a + b
    def plusNode(a: Exp[String], b: Exp[String]) = StringConcat(a, b)
  }

  implicit val asBool = new AsBool[Boolean] {
    def apply(e: Exp[Boolean]) = e
  }
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
  implicit def liftDouble(x: Double) : Exp[Double] = Const(x)
  implicit def liftInt(x: Int) : Exp[Int] = Const(x)
  implicit def liftBool(x: Boolean) : Exp[Boolean] = Const(x)
  implicit def liftString(x: String) : Exp[String] = Const(x)
  implicit def liftPair[A,B](pair: (Exp[A],Exp[B])) : Exp[(A,B)] = Pair[A,B](pair._1, pair._2)

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
}

trait AsBool[T] {
  def apply(e: Exp[T]) : Exp[Boolean]
}


