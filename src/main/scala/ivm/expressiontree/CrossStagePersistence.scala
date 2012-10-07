package ivm.expressiontree

import collection.mutable

/**
 * User: pgiarrusso
 * Date: 18/7/2012
 */
case class CSPVar(name: String, ctag: ClassTag[_], typ: TypeTag[_])

//This object is what is shared with Const. The split is done to speed up recompilation.
object CrossStagePersistence {
  private[expressiontree] val cspMap = new ScalaThreadLocal(mutable.ArrayBuffer[(Any, CSPVar)]())
  private[expressiontree] val varId = new Util.ThreadLocalIDGenerator

  private def addVar[T: ClassTag: TypeTag](value: T) = {
    val name = "x" + varId()
    cspMap.get() += (value -> CSPVar(name, classTag[T], typeTag[T]))
    name
  }
  private def persistUncached[T: ClassTag: TypeTag](value: T): Def[T] =
    NamedVar(addVar(value))

  /**
   * We have two different front-ends, persistConst and persistValue, and for very precise reasons:
   * persistConst can be used for ConstByIdentity nodes, for which it would be expensive to compute the actual
   * hash code for a cache lookup. persistValue can be used for everything else.
   * This implies a theoretical possibility that a value can be persisted twice, but it would not be a real problem.
   */
  private val persistConstCache = new mutable.HashMap[Const[_], Def[_]]

  def persistConst[T](c: Const[T]): Def[T] =
    persistConstCache.asInstanceOf[mutable.Map[Const[T], Def[T]]].getOrElseUpdate(c,
      persistUncached(c.x)(c.cTag, c.tTag))

  private val persistValueCache = new mutable.HashMap[Any, Def[_]]

  def persistValue[T: ClassTag: TypeTag](value: T): Def[T] =
    persistValueCache.asInstanceOf[mutable.Map[T, Def[T]]].getOrElseUpdate(value,
      persistUncached(value))

  def reset() {
    cspMap.get().clear()
    persistConstCache clear ()
    persistValueCache clear ()
  }
}

trait PersistValue[T, PersistValueT] extends Def[T] {
  def valueToPersist: PersistValueT
  implicit def cTagT: ClassTag[PersistValueT]
  implicit def tTagT: TypeTag[PersistValueT]
  protected var persistedValue: Def[PersistValueT] = _
  override def persistValues() {
    super.persistValues()
    persistedValue = CrossStagePersistence persistValue valueToPersist
  }
}

trait PersistClassS[T] extends PersistValue[T, Class[_]] {
  def classS: Class[_]
  override def valueToPersist = classS
  override def cTagT = classTag
  override def tTagT = typeTag
}
