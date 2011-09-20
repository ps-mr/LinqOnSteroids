package ivm.expressiontree

object Lifting extends SimpleOpenEncoding.MapOps with SimpleOpenEncoding.OpsExpressionTreeTrait {
  // these functions are explicitly not implicit :)
  def liftCall[Res: ClassManifest](id: Symbol, callfunc: () => Res) = new Call0(id,callfunc)
  def liftCall[A0, Res: ClassManifest](id: Symbol, callfunc: A0 => Res, arg0: Exp[A0]) = new Call1(id,callfunc, arg0)
  def liftCall[A0, A1, Res: ClassManifest](id: Symbol, callfunc: (A0, A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) =
     new Call2(id,callfunc, arg0, arg1)
  def liftCall[A0, A1, A2, Res: ClassManifest](id: Symbol, callfunc: (A0, A1, A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2]) =
     new Call3(id,callfunc, arg0, arg1, arg2)
  def liftCall[A0, A1, A2, A3, Res: ClassManifest](id: Symbol, callfunc: (A0, A1, A2, A3) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3]) =
     new Call4(id,callfunc, arg0, arg1, arg2, arg3)
  def liftCall[A0, A1, A2, A3, A4, Res: ClassManifest](id: Symbol, callfunc: (A0, A1, A2, A3, A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4]) =
     new Call5(id,callfunc, arg0, arg1, arg2, arg3, arg4)


  def liftFunc[S: ClassManifest, T: ClassManifest](f: Exp[S] => Exp[T]): Exp[S => T] = FuncExp(f)

  class NumOps[T](val t: Exp[T])(implicit val isNum: Numeric[T], val cm: ClassManifest[T]) {
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

  implicit def expToNumOps[T: Numeric: ClassManifest](t: Exp[T]) = new NumOps(t)
  implicit def expToOrderingOps[T: Ordering](t: Exp[T]) = new OrderingOps(t)
  implicit def expToStringOps(t: Exp[String]) = new StringOps(t)
  implicit def expToBooleanOps(t: Exp[Boolean]) = new BooleanOps(t)

  /*
   * In these definitions of toNumOps and toOrderingOps, implicit resolution fails because of ambiguity between liftOrd
   * and liftNum, if they are both declared - even if the ambiguity could easily be solved. The problem can be solved by
   * just having a polymorphic lift conversion. Other solutions are possible here but don't remove this ambiguity that
   * affects client code then.
   */
  implicit def toNumOps[T: Numeric: ClassManifest](t: T) = expToNumOps(t)
  implicit def toOrderingOps[T: Ordering: ClassManifest](t: T) = expToOrderingOps(t)
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

    implicit def liftCall1[A0, Res: ClassManifest](id: Symbol, f: A0 => Res):
      Exp[A0] => Exp[Res] = new Call1(id,f, _)
    implicit def liftCall2[A0, A1, Res: ClassManifest](id: Symbol, f: (A0, A1) => Res):
      (Exp[A0], Exp[A1]) => Exp[Res] = new Call2(id,f, _, _)
    implicit def liftCall3[A0, A1, A2, Res: ClassManifest](id: Symbol, f: (A0, A1, A2) => Res):
      (Exp[A0], Exp[A1], Exp[A2]) => Exp[Res] = new Call3(id,f, _, _, _)
    implicit def liftCall4[A0, A1, A2, A3, Res: ClassManifest](id: Symbol, f: (A0, A1, A2, A3) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3]) => Exp[Res] = new Call4(id,f, _, _, _, _)
    implicit def liftCall5[A0, A1, A2, A3, A4, Res: ClassManifest](id: Symbol, f: (A0, A1, A2, A3, A4) => Res):
      (Exp[A0], Exp[A1], Exp[A2], Exp[A3], Exp[A4]) => Exp[Res]= new Call5(id,f, _, _, _, _, _)
  }
}
