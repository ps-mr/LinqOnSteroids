package ivm.expressiontree

case class Join[T, S, TKey, TResult](colouter: QueryReifier[T],
  colinner: QueryReifier[S],
  outerKeySelector: FuncExp[T,TKey],
  innerKeySelector: FuncExp[S,TKey],
  resultSelector: FuncExp[(T, S),TResult]) extends QueryOp[TResult] {

  def children = Seq(colouter,colinner,outerKeySelector, innerKeySelector, resultSelector)
  def genericConstructor = (v) => Join(v(0).asInstanceOf[QueryReifier[T]], 
                                       v(1).asInstanceOf[QueryReifier[S]], 
                                       v(2).asInstanceOf[FuncExp[T,TKey]], 
                                       v(3).asInstanceOf[FuncExp[S,TKey]], 
                                       v(4).asInstanceOf[FuncExp[(T,S), TResult]])
  override def exec(isLazy: Boolean) = {
    // naive hash join algorithm
    val ci: scala.collection.Traversable[S] = colinner.exec()
    val co: scala.collection.Traversable[T] = colouter.exec()
    if (ci.size > co.size) {
      val map  = ci.groupBy(innerKeySelector.interpret())
      for (c <- co; d <- map(outerKeySelector.interpret()(c))) yield resultSelector.interpret()(c,d)  
    } else {
      val map  = co.groupBy(outerKeySelector.interpret())
      for (c <- ci; d <- map(innerKeySelector.interpret()(c))) yield resultSelector.interpret()(d,c)  
    }
  }
    
}
