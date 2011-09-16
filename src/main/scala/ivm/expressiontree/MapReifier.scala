package ivm.expressiontree


trait MapReifier[S,T] extends Exp[MapReifier[S,T]] {
  override def interpret() = this
  def apply(key: Exp[S]): QueryReifier[T] = MapApply(this, key)
  def exec : Map[S,Traversable[T]]
}

/* KO:
 * This is less general than it should be, since we hardcode here that we only
 * support maps whose values are lists.
 * It would be better if MapApply would extend Exp[T] and GroupBy would extend  MapReifier[T,QueryReifier[S]]
 * such that maps to arbitrary types are allowed.
 * However, this requires to solve the problem that all "QueryReifier" functions (map, flatMap etc.)
 * are available on Exp[Traversable[T]] and not only on QueryReifier[T]
 * Hopefully this will be possible once Paolo is done with the "major surgery" he promised ;)
*/
case class MapApply[S,T](map: MapReifier[S,T], key: Exp[S]) extends QueryReifier[T] {
  override def children = Seq(map,key)
  override def genericConstructor = (v) => MapApply(v(0).asInstanceOf[MapReifier[S,T]], v(1).asInstanceOf[Exp[S]])
  override def exec = map.exec(key.interpret())
}