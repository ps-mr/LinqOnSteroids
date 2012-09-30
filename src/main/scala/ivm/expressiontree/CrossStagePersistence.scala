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

  def persist[T: ClassTag: TypeTag](value: T): Exp[T] =
    NamedVar(CrossStagePersistence.addVar(value))
}

trait PersistValue[T] {
  this: Def[_] =>
  def valueToPersist: T
  implicit def cTagT: ClassTag[T]
  implicit def tTagT: TypeTag[T]
  protected var persistedValue: Exp[T] = _
  override def persistValues() {
    persistedValue = CrossStagePersistence persist valueToPersist
  }
}

trait PersistClassS extends PersistValue[Class[_]]{
  this: Def[_] =>
  def classS: Class[_]
  override def valueToPersist = classS
  override def cTagT = classTag
  override def tTagT = typeTag
}
