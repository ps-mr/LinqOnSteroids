package ivm.indexing

import ivm.expressiontree.{FuncExp, QueryReifier}
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class HashIndex[T,S](it: QueryReifier[T], f: FuncExp[T,S] ) extends HashMap[S,Traversable[T]] with Index[S,Traversable[T]] {
  {
    this ++= it.exec().groupBy(f.interpret())

  }
}

class HashIndex2[T1,T2,S](it: QueryReifier[T1],
                                     p1: FuncExp[T1,QueryReifier[T2]],
                                     f: FuncExp[(T1,T2),S]) extends HashMap[S,Traversable[(T1,T2)]]
                                                            with Index[S,Traversable[(T1,T2)]] {
  this ++= it.exec().flatMap( (x) =>
     p1.interpret()(x).exec().map( (y) => (x,y))).groupBy( (p) => ( f.interpret()(p)))
}

class HashIndex3[T1,T2,T3,S](it: QueryReifier[T1],
                                     p1: FuncExp[T1,QueryReifier[T2]],
                                     p2: FuncExp[T2,QueryReifier[T3]],
                                     f: FuncExp[(T1,T2,T3),S])
                                    extends HashMap[S,Traversable[(T1,T2,T3)]]
                                    with Index[S,Traversable[(T1,T2,T3)]]{
  this ++=
     it.exec().flatMap( (x) =>
        p1.interpret()(x).exec().map( (y) => (x,y))).
       flatMap( (z) => p2.interpret()(z._2).exec().map( (y) => (z._1,z._2,y))).
       groupBy( (p) => ( f.interpret()(p)))
}

class HashIndex4[T1,T2,T3,T4,S](it: QueryReifier[T1],
                                     p1: FuncExp[T1,QueryReifier[T2]],
                                     p2: FuncExp[T2,QueryReifier[T3]],
                                     p3: FuncExp[T3,QueryReifier[T4]],
                                     f: FuncExp[(T1,T2,T3,T4),S])
  extends HashMap[S,Traversable[(T1,T2,T3,T4)]]
  with Index[S,Traversable[(T1,T2,T3,T4)]] {
  this ++=
     it.exec().
       flatMap( (x) =>  p1.interpret()(x).exec().map( (y) => (x,y))).
       flatMap( (z) => p2.interpret()(z._2).exec().map( (y) => (z._1,z._2,y))).
       flatMap( (z) => p3.interpret()(z._3).exec().map( (y) => (z._1,z._2,z._3,y))).
       groupBy( (p) => ( f.interpret()(p)))
}

