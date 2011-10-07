package ivm
package expressiontree

// Variant of QueryReifier, which also sends event to derived collections. Note that this is not reified!
// XXX: we forget to add mutation operations. But see Queryable and QueryableTest. So make this a trait which is mixed in
// by Queryable.
trait QueryReifier[T] extends TravMsgSeqPublisher[T] with Exp[Traversable[T]] {
  type Pub <: QueryReifier[T]
  //XXX: the dummy parameter avoids view to override view in collections, when both are inherited (e.g. in Queryable).
  //The 'implicit' keyword allows to leave out the parameter list entirely.
  //def view(implicit dummy: Boolean = false) = new View[T, Traversable[T]](this)

  //subscription is a side effect, but query composition requires the passed functions to be pure.
  //In particular, when we pass Var inside a query, we execute these operations, but the results must just be inspected,
  //they must not become listeners - especially because they contain Var nodes.
  /*override def map[U](f: Exp[T] => Exp[U]): QueryReifier[U] =
    new MapOpMaintainerExp[T, U](this, FuncExp(f))
  override def withFilter(p: Exp[T] => Exp[Boolean]): QueryReifier[T] =
    new WithFilterMaintainerExp[T](this, FuncExp(p))
  override def flatMap[U](f: Exp[T] => Exp[QueryReifier[U]]): QueryReifier[U] =
    new FlatMapMaintainerExp[T, U](this, FuncExp(f))
  //XXX add join, and add union
  */
}
