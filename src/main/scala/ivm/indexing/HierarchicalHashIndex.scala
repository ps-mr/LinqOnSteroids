package ivm.indexing

import ivm.expressiontree.{FuncExp, QueryReifier}


class HierarchicalHashIndex2[T1,T2,S](it: QueryReifier[T1],
                                     p1: FuncExp[T1,QueryReifier[T2]],
                                     f: FuncExp[(T1,T2),S]) {
  val map : Map[S,Traversable[(T1,T2)]]= it.exec().flatMap( (x) =>
     p1.interpret()(x).exec().map( (y) => (x,y))).groupBy( (p) => ( f.interpret()(p)))
}

class HierarchicalHashIndex3[T1,T2,T3,S](it: QueryReifier[T1],
                                     p1: FuncExp[T1,QueryReifier[T2]],
                                     p2: FuncExp[T2,QueryReifier[T3]],
                                     f: FuncExp[(T1,T2,T3),S]) {
  val map : Map[S,Traversable[(T1,T2,T3)]]=
     it.exec().flatMap( (x) =>
        p1.interpret()(x).exec().map( (y) => (x,y))).
       flatMap( (z) => p2.interpret()(z._2).exec().map( (y) => (z._1,z._2,y))).
       groupBy( (p) => ( f.interpret()(p)))
}
