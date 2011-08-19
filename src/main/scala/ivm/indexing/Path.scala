package ivm.indexing

import ivm.expressiontree.{QueryReifier, FuncExp}

/*
 * This is a Scala variant of Lämmel et al's HList approach to typed heterogeneous lists
 */

// first type param is the type path, encoded as nested pair types. The second type param is the target type
sealed class Path[P,T]

case class EmptyPath[T]() extends Path[(T,Unit),T]

case class ConsPath[T1,T2,R,T](f: FuncExp[T1,QueryReifier[T2]], p: Path[(T2,R),T]) extends Path[(T1,(T2,R)),T]
