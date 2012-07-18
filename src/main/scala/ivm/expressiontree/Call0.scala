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
