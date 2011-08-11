package ivm
package expressiontree

import scala.collection.Iterator
import scala.collection.mutable.HashMap
import scala.collection.mutable
import indexing.HashIndex
import optimization.Optimization
import Lifting._

/*
 * TODO: we can move all these methods to Exp[T], by making them accept an implicit parameter only existing for
 * T = QueryReifier[U]. Or better, for T = Traversable[U] - that would replace
 * QueryReifier[U] by Exp[Traversable[U]]/Exp[TraversableView[U]]. How to make that extensible is not obvious.
 */
trait QueryReifier[T] extends Exp[Traversable[T]]  {
  def interpret = exec()

  //XXX: the dummy parameter avoids view to override view in collections, when both are inherited (e.g. in Queryable).
  //The 'implicit' keyword allows to leave out the parameter list entirely.
  def view(implicit dummy: Boolean = false) = new View(this)
  def force = new Force(this)

  def join[S,TKey,TResult](outercol: QueryReifier[S],
                           outerKeySelector: Exp[T] =>Exp[TKey],
                           innerKeySelector: Exp[S]=>Exp[TKey],
                           resultSelector: Exp[(T,S)] => Exp[TResult]) : QueryReifier[TResult] 
    = new Join[T,S,TKey,TResult](this, outercol, FuncExp(outerKeySelector),FuncExp(innerKeySelector), FuncExp(resultSelector));
  
  val indexes : mutable.Map[FuncExp[T,_],HashIndex[T,_]] = HashMap()
  def addIndex[S](f: FuncExp[T,S]) : Unit = {
    val nf = Optimization.normalize(f).asInstanceOf[FuncExp[T,S]]
    indexes += ((nf, new HashIndex(this,nf)))
  }
  def addIndex[S](f: Exp[T] => Exp[S]) : Unit = addIndex(FuncExp(f))
}
