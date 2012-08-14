package ivm.expressiontree

case class Call0[Res](name: Symbol, restId: Symbol, callfunc: () => Res) extends Arity0Exp[Res] with Call[Res] {
  def interpret() = callfunc()
}

class GlobalFuncCall1[A1, Res](override val name: Symbol,
                               override val prefix: String,
                               callfunc: (A1) => Res,
                               override val t1: Exp[A1])
    extends Call1[A1, Res](name, Symbol(""), callfunc, t1)
    with Arity1OpTrait[Exp[A1], Res, GlobalFuncCall1[A1, Res]] with PrefixPrinting
{
  override def copy(t1: Exp[A1]) = new GlobalFuncCall1(name, prefix, callfunc, t1)
}

class GlobalFuncCall2[A1, A2, Res](override val name: Symbol,
                               override val prefix: String,
                               callfunc: (A1, A2) => Res,
                               override val t1: Exp[A1], override val t2: Exp[A2])
    extends Call2[A1, A2, Res](name, Symbol(""), callfunc, t1, t2)
    with Arity2OpTrait[Exp[A1], Exp[A2], Res, GlobalFuncCall2[A1, A2, Res]] with PrefixPrinting
{
  override def copy(t1: Exp[A1],t2: Exp[A2]) = new GlobalFuncCall2(name, prefix, callfunc, t1, t2)
}
