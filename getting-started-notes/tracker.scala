/**
 * Dummy class for tracing things via command+B
 */

import ivm.expressiontree._

trait Foo[T] {

  val c : ConstByIdentity[T]

  val c1: Contains[T]

  val d : Def[T]

  val e : Exp[T]

  val f : FunSym[T, T]

  val f1: Filter[T, T]

  val l : LiftTuple2[T, T]

  val m : MapNode[T, T, T, T]

  val s : Sym[T]

}