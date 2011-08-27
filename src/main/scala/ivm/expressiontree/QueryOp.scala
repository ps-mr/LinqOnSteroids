package ivm.expressiontree

/**
 * An expression tree node operating on expressions. Interpretation returns the node itself because only exec will
 * produce the actual result.
 */
//@deprecated("Use QueryReifier directly")
trait QueryOp[T] extends QueryReifier[T]
