package ivm.indexing

import ivm.expressiontree.{FuncExp, QueryReifier}
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

/*
 * These indexes generalize path indexes as described in the following paper:
 * Bertino, E., Kim, W.: Indexing Techniques for Queries on Nested Objects. IEEE Trans. Knowl. Data Eng.(1989) 196-214
 *
 * These indexes are more general, because the paths need not consist of attribute names
 * but are the result of arbitrary function computations
 *
 * Note: It would also be useful to have a generalized form of the multi indexes as described in the paper above.
 * Lookup is slower but maintenance is cheaper. Furthermore, they presumably work well together with
 * sharing in the object graph, i.e., index replication can be avoided.
 */

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

class PathIndex[T1,P,S](it: QueryReifier[T1], path: Path[(T1,P)], f: FuncExp[(T1,P),S])
   extends HashMap[S,Traversable[(T1,P)]]
   with Index[S,Traversable[(T1,P)]] {
   private def traversePath[T,R](c: Traversable[T], p: Path[(T,R)]) : Traversable[(T,R)] = {
        p match {
          case EmptyPath() => c.map( (x) => (x,()))
          // next is the explicitly typed version, which looks worse than the uncommented one but was easier to write
/*          case cp: ConsPath[_,t2,r] =>
           c.flatMap( (x) => traversePath[t2,r](cp.f.interpret()(x).exec(), cp.p).map ( (y) => (x,y)) )*/
          case ConsPath(f,path) =>
           c.flatMap( (x) => traversePath(f.interpret()(x).exec(), path).map ( (y) => (x,y)) )
        }
   }

  this ++= traversePath(it.exec(), path).groupBy( (p) => f.interpret()(p))
}