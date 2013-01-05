trait GeneralIntersection {
  type Bag[X] <: Traversable[X] //This should be an interface.
  def toMapBag[A]: Bag[A] => MapBag[A]

  type MapBag[A] = Map[A, Int] //This forms a group, as opposed to Map[A, Nat]

  //A sort of join with post-processing for multiplicities. Possibly it could be useful to decompose this into a join + maps + toMapBag for further optimizations.
  def zipMultWith[A] (bag1: Bag[A], bag2: Bag[A])(mergeMults: (A, Int, Int) => Int): MapBag[A] = {
    //val (mBag1, mBag2) = (bag1, bag2) map toMapBag //Probably illegal

    val (mBag1, mBag2) = (toMapBag(bag1), toMapBag(bag2))

    for ((el, mult1) <- mBag1; mult2 = mBag2(el))
      yield (el, mergeMults(el, mult1, mult2))
  } //How to maintain this? Whenever an element is added/removed, reapply the function to recompute its multiplicity. With this interface, when an element is changed the function must be re-run, while that maybe wouldn't be the case if mergeMults didn't get a parameter of type A? Wrong: if an element changes, it does so in 1 collection only, so one needs to apply the  it with another multiplicity.

  //bag1 intersect bag2 ==> intersect(bag1, bag2)
  def intersect[A](bag1: Bag[A], bag2: Bag[A]) =
    zipMultWith(bag1, bag2) { (_, mult1, mult2) => mult1 min mult2 }

  //bag1 union bag2 ==> union(bag1, bag2)
  def union[A](bag1: Bag[A], bag2: Bag[A]) =
    zipMultWith(bag1, bag2) { (_, mult1, mult2) => mult1 max mult2 }

  //bag1 filter (bag2 contains) ==> rightIntersect(bag1, bag2)
  def rightIntersect[A](bag1: Bag[A], bag2: Bag[A]) =
    zipMultWith(bag1, bag2) {
      (_, mult1, mult2) =>
	if (mult2 > 0) mult1 else 0
    }
}
