package ivm.collections

import collection.IterableLike

/**
 * User: pgiarrusso
 * Date: 13/12/2012
 */

trait BagLike[A, +Repr] extends IterableLike[A, Repr] {
  //Copy/alter SetLike/GenSetLike into this.
  def empty: Repr
  def contains(elem: A): Boolean
  def count(elem: A): Int
  def apply(elem: A) = count(elem)
  def +(elem: A): Repr
  def -(elem: A): Repr

  /** Computes the intersection between this bag and another bag.
    *
    *  @param   that  the bag to intersect with.
    *  @return  a new bag consisting of all elements that are both in this
    *  bag and in the given bag `that`.
    */
  def intersect(that: Bag[A]): Repr// = this filter that

  /** Computes the intersection between this bag and another bag.
    *
    *  '''Note:'''  Same as `intersect`.
    *  @param   that  the bag to intersect with.
    *  @return  a new bag consisting of all elements that are both in this
    *  bag and in the given bag `that`.
    */
  def &(that: Bag[A]): Repr = this intersect that

  /** Computes the union between of bag and another bag.
    *
    *  @param   that  the bag to form the union with.
    *  @return  a new bag consisting of all elements that are in this
    *  bag or in the given bag `that`.
    */
  def union(that: Bag[A]): Repr

  /** Computes the union between this bag and another bag.
    *
    *  '''Note:'''  Same as `union`.
    *  @param   that  the bag to form the union with.
    *  @return  a new bag consisting of all elements that are in this
    *  bag or in the given bag `that`.
    */
  def | (that: Bag[A]): Repr = this union that

  /** Computes the difference of this bag and another bag.
    *
    *  @param that the bag of elements to exclude.
    *  @return     a bag containing those elements of this
    *              bag that are not also contained in the given bag `that`.
    */
  def diff(that: Bag[A]): Repr

  /** The difference of this bag and another bag.
    *
    *  '''Note:'''  Same as `diff`.
    *  @param that the bag of elements to exclude.
    *  @return     a bag containing those elements of this
    *              bag that are not also contained in the given bag `that`.
    */
  def &~(that: Bag[A]): Repr = this diff that

  /** Tests whether this bag is a subset of another bag.
    *
    *  @param that  the bag to test.
    *  @return     `true` if this bag is a subset of `that`, i.e. if
    *              every element of this bag is also an element of `that`.
    */
  def subsetOf(that: Bag[A]): Boolean// = this forall that

  /** Compares this bag with another object for equality.
    *
    *  '''Note:''' This operation contains an unchecked cast: if `that`
    *        is a bag, it will assume with an unchecked cast
    *        that it has the same element type as this bag.
    *        Any subsequent ClassCastException is treated as a `false` result.
    *  @param that the other object
    *  @return     `true` if `that` is a bag which contains the same elements
    *              as this bag.
    */
  override def equals(that: Any): Boolean = that match {
    case that: Bag[_] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) &&
      (try this subsetOf that.asInstanceOf[Bag[A]]
      catch { case ex: ClassCastException => false })
    case _ =>
      false
  }

  override def hashCode(): Int// = (this map (_.hashCode)).sum
}

trait Bag[T] extends BagLike[T, Bag[T]]
