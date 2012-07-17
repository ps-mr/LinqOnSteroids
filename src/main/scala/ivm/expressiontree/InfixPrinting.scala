package ivm.expressiontree

/**
 * User: pgiarrusso
 * Date: 11/7/2012
 */

trait InfixPrinting {
  this: Exp[_] =>
  override def toCode: String =
    /*children.head.toCode +
    //"." +
    " " +
    operator + children.tail.map(_.toCode).mkString("(", ", ", ")")*/
    "(%s).%s%s" format (children.head.toCode, operator, if (children.tail.isEmpty) "" else children.tail map (_.toCode) mkString (" (", ", ", ")"))
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
