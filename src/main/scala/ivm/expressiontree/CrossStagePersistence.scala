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

  def persist[T: ClassTag: TypeTag](value: T) =
    NamedVar(CrossStagePersistence.addVar(value))
}
