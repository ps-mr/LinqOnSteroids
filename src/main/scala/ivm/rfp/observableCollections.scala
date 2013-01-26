package ivm.rfp

import collection.mutable.{Buffer, Set}

// This is for a modifiable sequence. Actually, it's a problem since we don't preserve ordering, but we should.
// On the one hand, this interface matches a relation in the closest way.
trait ObservableBuffer[T] extends Buffer[T] with TravMsgSeqPublisher[T, ObservableBuffer[T]] {
  //type Pub <: ObservableBuffer[T]
  //XXX: we might want to make this still less ad-hoc
  private[this] var silenceNotifications = false

  override protected[this] def publish(evt: Seq[Message[Traversable[T]]]) {
    if (!silenceNotifications)
      super.publish(evt)
  }

  abstract override def clear() = {
    //publish(Script(this.map(Remove(_))))
    publish(Reset)
    super.clear()
  }
  abstract override def += (el: T) = {
    publish(Include(el))
    super.+=(el)
  }
  abstract override def +=: (el: T) = {
    publish(Include(el))
    super.+=:(el)
  }
  abstract override def update(n: Int, newelem: T) = {
    publish(Update(this(n), newelem))
    super.update(n, newelem)
  }
  abstract override def remove(n: Int) = {
    publish(Remove(this(n)))
    super.remove(n)
  }
  abstract override def insertAll(n: Int, iter: Traversable[T]) = {
    // Where we call toSeq is important - by creating the sequence once, we guarantee that the order of events is the
    // same as the actual insertion order.
    val seq = iter.toSeq
    publish(seq.map(Include(_)))
    super.insertAll(n, seq)
  }
  /*
  We need to override also ++=, whose implementation uses += only sometimes. Since this implementation is inspired by
  ObservableBuffer, we also reproduce a bug of it [1] - arguably, it's rather a bug in the specs for ArrayBuffer, which
  does not explain how to handle this.
  [1] https://issues.scala-lang.org/browse/SI-4461
  From ArrayBuffer:
  override def ++=(xs: TraversableOnce[A]): this.type = xs match {
    case v: collection.IndexedSeqLike[_, _] =>
      val n = v.length
      ensureSize(size0 + n)
      v.copyToArray(array.asInstanceOf[scala.Array[Any]], size0, n)
      size0 += n
      this
    case _ =>
      super.++=(xs)
  }
  super.++= invokes
  */
  abstract override def ++=(xs: TraversableOnce[T]): this.type = {
    val seq = xs.toSeq
    publish(seq map (Include(_)))
    assert(silenceNotifications == false)
    //super.++= is defined in terms of +=, so we want to prevent += from
    //publishing updates.
    silenceNotifications = true
    val res = super.++=(seq)
    silenceNotifications = false
    res
  }
}

// Here we don't get info about replaced elements. However, for observable elements, we should still register ourselves
// to forward them.
trait ObservableSet[T] extends Set[T] with TravMsgSeqPublisher[T, ObservableSet[T]] {
  //type Pub <: ObservableSet[T]
  //XXX: we might want to make this less ad-hoc
  private[this] var silenceNotifications = false
  abstract override def clear() = {
    //publish(Script(this.map(Remove(_))))
    if (!silenceNotifications)
      publish(Reset)
    super.clear()
  }
  abstract override def += (el: T) = {
    // If we `Include` an element twice, we'll need to `Remove` it twice as well before e.g. the final IncrementalResult
    // instance realizes that it should disappear.
    if (!silenceNotifications && !this(el))
      publish(Include(el))
    super.+=(el)
  }

  //To get bulk updates, we want to override ++= as well.
  abstract override def ++= (xs: TraversableOnce[T]) = {
    //We need to prefilter elements, to avoid Include'ing elements which are
    //already there.
    val newEls = (for (x <- xs; if !(this contains x)) yield x).toSeq
    publish(newEls map (Include(_)))
    assert(silenceNotifications == false)
    //super.++= is defined in terms of +=, so we want to prevent += from
    //publishing updates.
    silenceNotifications = true
    val res = super.++=(newEls)
    silenceNotifications = false
    res
  }

  abstract override def -= (el: T) = {
    if (!silenceNotifications && this(el))
      publish(Remove(el))
    super.-=(el)
  }

  abstract override def --= (xs: TraversableOnce[T]) = {
    //We need to prefilter elements, to avoid Remove'ing elements which are
    //already not there.
    val newEls = (for (x <- xs; if this contains x) yield x).toSeq
    // To understand the heuristic, imagine maintaining MapNode(this, f) in response to this --= xs.
    // To minimize the invocations of f needed for IVM, this is the correct strategy.
    if (newEls.size > size/2)
      doRemoveAndRebuild(newEls) //We could use xs directly here, and save computing newEls - but we need that for the
      // heuristic
    else
      doRemoveAndNotify(newEls)
    this
  }

  private def doRemoveAndRebuild(newEls: Seq[T]) {
    publish(Reset)
    publish((this -- newEls).toSeq map (Include(_))) // Here we compute this -- newEls and throw it away;
    // doing the notification afterwards
    assert(silenceNotifications == false)
    //super.--= is defined in terms of -=, so we want to prevent -= from
    //publishing updates.
    silenceNotifications = true
    super.--=(newEls)
    silenceNotifications = false
  }

  private def doRemoveAndNotify(newEls: Seq[T]) {
    publish(newEls map (Remove(_)))
    assert(silenceNotifications == false)
    //super.--= is defined in terms of -=, so we want to prevent -= from
    //publishing updates.
    silenceNotifications = true
    super.--=(newEls)
    silenceNotifications = false
  }
}
