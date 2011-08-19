package ivm.indexing

import ivm.expressiontree.{QueryReifier, FuncExp}

/*
 * This is a Scala variant of Lämmel et al's HList approach to typed heterogeneous lists
 */

sealed class Path[T]

case class EmptyPath[T]() extends Path[(T,Unit)]

case class ConsPath[T1,T2,R](f: FuncExp[T1,QueryReifier[T2]], p: Path[(T2,R)]) extends Path[(T1,(T2,R))]
