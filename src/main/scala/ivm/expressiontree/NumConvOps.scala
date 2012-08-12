package ivm.expressiontree

trait NumConvOps {
  this: LiftingConvs with FunctionOps =>

  implicit def expInt2Double[T](t: Exp[Int]): Exp[Double] =
    convLift(t, 'Int2Double, "Int.int2double")

  implicit def expInt2Long[T](t: Exp[Int]): Exp[Long] =
    convLift(t, 'Int2Long, "Int.int2long")

  implicit def expInt2Float[T](t: Exp[Int]): Exp[Float] =
    convLift(t, 'Int2Float, "Int.int2float")

  implicit def int2ExpDouble = convFromBase[Int, Double](_)
  implicit def int2ExpLong = convFromBase[Int, Long](_)
  implicit def int2ExpFloat = convFromBase[Int, Float](_)
}
