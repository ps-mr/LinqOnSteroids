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
  case class PairHelper[A,B](p: Exp[(A,B)]) {
    val _1 = Proj1(p)
    val _2 = Proj2(p)
  }
  implicit def toPairHelper[A,B](e: Exp[(A,B)]) : PairHelper[A,B] = PairHelper(e)

  // these functions are explicitly not implicit :)
  def liftCall[Res](callfunc: () => Res) = Call0(callfunc)
  def liftCall[A0,Res](callfunc: A0 => Res, arg0: Exp[A0])  = Call1(callfunc,arg0)
  def liftCall[A0,A1,Res](callfunc: (A0,A1) => Res, arg0: Exp[A0], arg1: Exp[A1]) = Call2(callfunc,arg0,arg1)
  def liftCall[A0,A1,A2,Res](callfunc: (A0,A1,A2) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2])  = Call3(callfunc,arg0,arg1,arg2)
  def liftCall[A0,A1,A2,A3,Res](callfunc: (A0,A1,A2,A3) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3])  = Call4(callfunc,arg0,arg1,arg2,arg3)
  def liftCall[A0,A1,A2,A3,A4,Res](callfunc: (A0,A1,A2,A3,A4) => Res, arg0: Exp[A0], arg1: Exp[A1], arg2: Exp[A2], arg3: Exp[A3], arg4: Exp[A4])  = Call5(callfunc,arg0,arg1,arg2,arg3,arg4)


  
  def liftFunc[S,T](f: Exp[S] => Exp[T]) : Exp[S => T] = FuncExp(f)
  implicit def liftDouble(x: Double) : Exp[Double] = Const(x)
  implicit def liftInt(x: Int) : Exp[Int] = Const(x)
  implicit def liftBool(x: Boolean) : Exp[Boolean] = Const(x)
  implicit def liftString(x: String) : Exp[String] = Const(x)
  implicit def liftPair[A,B](pair: (Exp[A],Exp[B])) : Exp[(A,B)] = Pair[A,B](pair._1, pair._2)
  

  
}
