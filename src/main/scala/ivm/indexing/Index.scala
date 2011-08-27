package ivm.indexing

/**
 * Created by IntelliJ IDEA.
 * User: klaus
 * Date: 13.08.11
 * Time: 14:59
 * To change this template use File | Settings | File Templates.
 */
import scala.collection.Map

trait Index[S, +T] extends Map[S, T] {
  override def toString() = "Index"
}