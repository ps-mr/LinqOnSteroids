package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 11/7/2012
 */

trait InfixPrinting {
  this: Exp[_] =>
  override def toCode: String = {
    val isSymbol = operator.matches("[A-Za-z_][A-Za-z_0-9]*")
    val hasArgs = children.tail.nonEmpty
    "(" + children.head.toCode + ")" + (if (isSymbol) '.' else ' ') + operator +
      (if (hasArgs) (if (!isSymbol) " " else "") + (children.tail map (_.toCode) mkString ("(", ", ", ")")) else "")
  }
  def operator: String
}

trait PrefixPrinting {
  this: Exp[_] =>
  override def toCode = children.map(_.toCode).mkString(prefix + "(", ", ", ")")
  def prefix: String
}

trait FormatPrinting {
  this: Exp[_] =>
  override def toCode = formatStr format (children map (_.toCode): _*)
  def formatStr: String
}
