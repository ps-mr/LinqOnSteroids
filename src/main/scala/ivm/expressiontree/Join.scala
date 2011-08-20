package ivm.expressiontree

case class Join[T, S, TKey, TResult](colouter: Exp[Traversable[T]],
  colinner: Exp[Traversable[S]],
  outerKeySelector: FuncExp[T,TKey],
  innerKeySelector: FuncExp[S,TKey],
  resultSelector: FuncExp[(T, S),TResult]) extends Exp[Traversable[TResult]] {

  def children = Seq(colouter,colinner,outerKeySelector, innerKeySelector, resultSelector)
  def genericConstructor = (v) => Join(v(0).asInstanceOf[Exp[Traversable[T]]],
                                       v(1).asInstanceOf[Exp[Traversable[S]]],
                                       v(2).asInstanceOf[FuncExp[T,TKey]],
                                       v(3).asInstanceOf[FuncExp[S,TKey]],
                                       v(4).asInstanceOf[FuncExp[(T,S),TResult]])
  override def interpret() = {
    // naive hash join algorithm
    val ci: scala.collection.Traversable[S] = colinner.interpret()
    val co: scala.collection.Traversable[T] = colouter.interpret()
    if (ci.size > co.size) {
      val map  = ci.groupBy(innerKeySelector.interpret())
      for (c <- co; d <- map(outerKeySelector.interpret()(c))) yield resultSelector.interpret()(c,d)  
    } else {
      val map  = co.groupBy(outerKeySelector.interpret())
      for (c <- ci; d <- map(innerKeySelector.interpret()(c))) yield resultSelector.interpret()(d,c)  
    }
  }
    
}
