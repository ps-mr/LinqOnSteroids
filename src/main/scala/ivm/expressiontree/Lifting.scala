package ivm.expressiontree

object Lifting extends SimpleOpenEncoding.MapOps with SimpleOpenEncoding.SetOps with SimpleOpenEncoding.OpsExpressionTreeTrait with SimpleOpenEncoding.TypeFilterOps {
  def liftFunc[S, T](f: Exp[S] => Exp[T]): Exp[S => T] = FuncExp(f)

  implicit def arrayToExpSeq[T](x: Array[T]) = (x: Seq[T]): Exp[Seq[T]]

  class NumericOps[T: Numeric](t: Exp[T]) {
    def +(that: Exp[T]): Exp[T] = Plus(this.t, that)
    def *(that: Exp[T]): Exp[T] = Times(this.t, that)
    def -(that: Exp[T]): Exp[T] = onExp(this.t, that)('NumericOps$minus, implicitly[Numeric[T]].minus(_, _))
  }

  class FractionalOps[T: Fractional](t: Exp[T]) {
    def /(that: Exp[T]): Exp[T] = onExp(this.t, that)('FractionalOps$div, implicitly[Fractional[T]].div(_, _))
  }

  class IntegralOps[T: Integral](t: Exp[T]) {
    def %(that: Exp[T]): Exp[T] = onExp(this.t, that)('IntegralOps$mod, implicitly[Integral[T]].rem(_, _))
  }

  class OrderingOps[T: Ordering](t: Exp[T]) {
    //XXX: we probably need to use distinguished nodes for these operations, to be able to use indexes for them.
    def <=(that: Exp[T]): Exp[Boolean] = LEq(this.t, that)
    def <(that: Exp[T]): Exp[Boolean] = onExp(this.t, that)('OrderingOps$lt, implicitly[Ordering[T]].lt(_, _))
    def >(that: Exp[T]): Exp[Boolean] = onExp(this.t, that)('OrderingOps$gt, implicitly[Ordering[T]].gt(_, _))
    def >=(that: Exp[T]): Exp[Boolean] = onExp(this.t, that)('OrderingOps$gteq, implicitly[Ordering[T]].gteq(_, _))
  }

  class StringOps(t: Exp[String]) {
    def +(that: Exp[String]) = StringConcat(t, that)
  }

  class BooleanOps(b: Exp[Boolean]) {
    def &&(that: Exp[Boolean]) = And(b, that)
    def ||(that: Exp[Boolean]) = Or(b, that)
    def unary_! = Not(b)
  }

  implicit def expToNumOps[T: Numeric](t: Exp[T]) = new NumericOps(t)
  implicit def expToIntegralOps[T: Integral](t: Exp[T]) = new IntegralOps(t)
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
  /*implicit def toNumOps[T: Numeric](t: T): NumericOps[T] = Const(t)
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

    implicit def liftCall1[A0, Res](id: Symbol, f: A0 => Res):
      Exp[A0] => Exp[Res] = new Call1(id,f, _)
    implicit def liftCall2[A0, A1, Res](id: Symbol, f: (A0, A1) => Res):
      (Exp[A0], Exp[A1]) => Exp[Res] = new Call2(id,f, _, _)
    implicit def liftCall3[A0, A1, A2, Res](id: Symbol, f: (A0, A1, A2) => Res):
      (Exp[A0], Exp[A1], Exp[A2]) => Exp[Res] = new Call3(id,f, _, _, _)
    implicit def liftCall4[A0, A1, A2, A3, Res](id: Symbol, f: (A0, A1, A2, A3) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3]) => Exp[Res] = new Call4(id,f, _, _, _, _)
    implicit def liftCall5[A0, A1, A2, A3, A4, Res](id: Symbol, f: (A0, A1, A2, A3, A4) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3], Exp[A4]) => Exp[Res]= new Call5(id,f, _, _, _, _, _)
  }

  // maybe this is not the best place to define this function
  def filterByType[S: Manifest]: Exp[PartialFunction[Any,S]] = new PartialFuncExp( (x) => x.ifInstanceOf[S])
}
