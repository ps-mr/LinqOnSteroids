package ivm.expressiontree

import collection.mutable.ArrayBuffer

/**
 * User: pgiarrusso
 * Date: 18/7/2012
 */
case class CSPVar(name: String, ctag: ClassTag[_], typ: TypeTag[_])

//This object is what is shared with Const. The split is done to speed up recompilation.
object CrossStagePersistence {
  private[expressiontree] val cspMap = new ScalaThreadLocal(ArrayBuffer[(Any, CSPVar)]())
  private[expressiontree] val varId = new Util.ThreadLocalIDGenerator

  def addVar[T: ClassTag: TypeTag](value: T) = {
    val name = "x" + varId()
    cspMap.get() += (value -> CSPVar(name, classTag[T], typeTag[T]))
    name
  }

  def persist[T: ClassTag: TypeTag](value: T): Def[T] =
    NamedVar(CrossStagePersistence.addVar(value))
}

trait PersistValue[T, PersistValueT] extends Def[T] {
  def valueToPersist: PersistValueT
  implicit def cTagT: ClassTag[PersistValueT]
  implicit def tTagT: TypeTag[PersistValueT]
  protected var persistedValue: Def[PersistValueT] = _
  override def persistValues() {
    super.persistValues()
    persistedValue = CrossStagePersistence persist valueToPersist
  }
}

trait PersistClassS[T] extends PersistValue[T, Class[_]] {
  def classS: Class[_]
  override def valueToPersist = classS
  override def cTagT = classTag
  override def tTagT = typeTag
}
