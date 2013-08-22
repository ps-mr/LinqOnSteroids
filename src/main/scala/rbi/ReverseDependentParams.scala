package rbi

/*

Time for the details. Sorry for the length, but the main point comes early.

1) `q` still returns `api.Universe#Tree`, but we can make the interface more specific:

```scala
def q[U <: api.Universe](tree: U#Tree)(implicit u: tree.U): u.Tree = tree.asInstanceOf[u.Tree]
```

Now we have a precise return type.

2) We have an ugly cast. That cast shouldn't be needed, but here Scalac doesn't know that in fact tree has type u.Tree. It knows this in the universe itself, so we can define a conversion method there without a cast.

```scala
object api { class Universe { class Tree { type U = Universe.this.type; def toUTree(u: U): u.Tree = this } } }
def q[U <: api.Universe](tree: U#Tree)(implicit u: tree.U): u.Tree = tree.toUTree(u)
```

3) Moreover, we don't even need type inference anymore:

```scala
def q(tree: api.Universe#Tree)(implicit u: tree.U): u.Tree = tree.toUTree(u)
```

To show that we can take multiple parameters dependent on the same implicit parameter, let's also encode sameU the same way:

```scala
def sameU2(t1: api.Universe#Tree, t2: api.Universe#Tree)(implicit u: t1.U with t2.U): (u.Tree, u.Tree) = (t1.toUTree(u), t2.toUTree(u))
```

Notice the `with`: we rely on `A1 with A2` equaling `A1` when `A1 == A2`, and I dare say Scalac doesn't fail us. The only problem is when an argument (`t1` or `t2`) was upcast to `api.Universe#Tree`: in that case, `sameU2` incorrectly compiles, and `toUTree` allows casting `t2.U` to `t1.U` without an explicit cast. That is, we have a soundness bug (a known one?).

```scala
scala> sameU2(t1, qBadT2) //Does not fail either - doh!
res14: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@450a3962)
scala> (t1, t2)
res15: (u1.Tree, u2.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@450a3962)
```

What the typechecker should do instead seems conceptually clear:

```scala
scala> iu1: t1.U with qBadT2.U //shouldn't compile
res16: t1.U with qBadT2.U = api$Universe@54405a01

scala> iu1: t1.U with t2.U //should fail, and it does
<error>

scala> val upcastT2 = t2: api.Universe#Tree

scala> iu1: t1.U with upcastT2.U //should fail, but it doesn't!
res21: t1.U with upcastT2.U = api$Universe@54405a01
```

The problem is that `upcastT2.U` seems simplified away or at least ignored (maybe first to `api.Universe`?), but it should be treated like an existential and not simplified: `iu1: t1.U with upcastT2.U` should not be accepted.

Last notes:

* I'd like to just declare two implicits at once by writing:
```scala
implicit val (u1, u2) = (new api.Universe, new api.Universe)
```
but it doesn't work - `u1` and `u2` aren't found by implicit resolution then.

* Passing the universe to toUTree bugs me, but it seems Scalac doesn't have enough reasoning power to avoid this:

```scala
scala> object api { class Universe { class Tree { type U = Universe.this.type; def toUTree: Universe.this.Tree = this }}}
defined module api

scala> def q[U <: api.Universe](tree: U#Tree)(implicit u: tree.U): u.Tree = tree.toUTree
<console>:41: error: type mismatch;
 found   : U#Tree
 required: u.Tree
       def q[U <: api.Universe](tree: U#Tree)(implicit u: tree.U): u.Tree = tree.toUTree
                                                                                 ^
```

* A complete transcript follows, but probably you want to take a look at https://gist.github.com/Blaisorblade/6259337 instead.

```scala

scala> object api { class Universe { class Tree { type U = Universe.this.type; def toUTree(u: U): u.Tree = this } } }
defined module api

scala> def q(tree: api.Universe#Tree)(implicit u: tree.U): u.Tree = tree.toUTree(u)
q: (tree: api.Universe#Tree)(implicit u: tree.U)u.Tree

scala> def sameU(u: api.Universe)(t1: u.Tree, t2: u.Tree) = (t1, t2)
sameU: (u: api.Universe)(t1: u.Tree, t2: u.Tree)(u.Tree, u.Tree)

scala> def sameU2(t1: api.Universe#Tree, t2: api.Universe#Tree)(implicit u: t1.U with t2.U): (u.Tree, u.Tree) = (t1.toUTree(u), t2.toUTree(u))
sameU2: (t1: api.Universe#Tree, t2: api.Universe#Tree)(implicit u: t1.U with t2.U)(u.Tree, u.Tree)

scala> val (u1, u2) = (new api.Universe, new api.Universe)
u1: api.Universe = api$Universe@54405a01
u2: api.Universe = api$Universe@4cd522dd

scala> val (t1, t2) = (new u1.Tree, new u2.Tree)
t1: u1.Tree = api$Universe$Tree@331d3edd
t2: u2.Tree = api$Universe$Tree@450a3962

scala> implicit val iu1: u1.type = u1
iu1: u1.type = api$Universe@54405a01

scala> implicit val iu2: u2.type = u2
iu2: u2.type = api$Universe@4cd522dd

scala> //Test sameU2

scala> sameU2(t1, t2) //Fails
<console>:16: error: could not find implicit value for parameter u: t1.U with t2.U
              sameU2(t1, t2) //Fails
                    ^

scala> sameU2(t1, t1)
res1: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, new u1.Tree)
res2: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@7b8d343a)

scala> sameU2(new u1.Tree, t1)
res3: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@42049d11,api$Universe$Tree@331d3edd)

scala> //This was transform:

scala> def qBad[U <: api.Universe](tree: U#Tree): U#Tree = tree
qBad: [U <: api.Universe](tree: U#Tree)U#Tree

scala> q(t1)
res4: iu1.Tree = api$Universe$Tree@331d3edd

scala> q(t2)
res5: iu2.Tree = api$Universe$Tree@450a3962

scala> //Call transform and q inline to help type inference:

scala> sameU(u1)(t1, qBad(t1))
res6: (u1.Tree, u1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU(u1)(t1, q(t1))
res7: (u1.Tree, u1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, qBad(t1))
res8: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, q(t1))
res9: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala>

scala> //Call transform and q *not* inline, to avoid helping type inference too much:

scala>

scala> val qBadT1 = qBad(t1)
qBadT1: api.Universe#Tree = api$Universe$Tree@331d3edd

scala> val qBadT2 = qBad(t2)
qBadT2: api.Universe#Tree = api$Universe$Tree@450a3962

scala> val qT1 = q(t1)
qT1: iu1.Tree = api$Universe$Tree@331d3edd

scala> sameU(u1)(t1, qBadT1) //Fails, although somewhat spuriously.
<console>:17: error: type mismatch;
 found   : api.Universe#Tree
 required: u1.Tree
              sameU(u1)(t1, qBadT1) //Fails, although somewhat spuriously.
                            ^

scala> sameU(u1)(t1, qT1) //Succeeds! Yeah!
res11: (u1.Tree, u1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, qBadT1) //Does not fail, even though qBadT1 was upcast. Is this good?
res12: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, qT1)
res13: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@331d3edd)

scala> sameU2(t1, qBadT2) //Does not fail either - doh!
res14: (iu1.Tree, iu1.Tree) = (api$Universe$Tree@331d3edd,api$Universe$Tree@450a3962)
```

 */

object ReverseDependentParams {
  object api {
    class Universe {
      class Tree {
        type U = Universe.this.type
        def toUTree(u: U): u.Tree = this
      }
    }
  }

  def q[U <: api.Universe](tree: U#Tree)(implicit u: tree.U): u.Tree = tree.toUTree(u)

  def sameU(u: api.Universe)(t1: u.Tree, t2: u.Tree) = (t1, t2)
  def sameU2(t1: api.Universe#Tree, t2: api.Universe#Tree)(implicit u: t1.U with t2.U): (u.Tree, u.Tree) = (t1.toUTree(u), t2.toUTree(u))

  val (u1, u2) = (new api.Universe, new api.Universe)
  val (t1, t2) = (new u1.Tree, new u2.Tree)
  implicit val iu1: u1.type = u1
  implicit val iu2: u2.type = u2

  //Old API
  def qBad[U <: api.Universe](tree: U#Tree): U#Tree = tree

  //Test sameU2
  //sameU2(t1, t2) //Fails
  sameU2(t1, t1)
  sameU2(t1, new u1.Tree)
  sameU2(new u1.Tree, t1)

  val qBadT1 = qBad(t1)
  val qBadT2 = qBad(t2)
  val qT1 = q(t1)

  //Call transform and q inline to help type inference:
  sameU(u1)(t1, qBad(t1))
  sameU(u1)(t1, q(t1))

  sameU2(t1, qBad(t1))
  sameU2(t1, q(t1))

  //Call transform and q *not* inline, to avoid helping type inference too much:

  //sameU(u1)(t1, qBadT1) //Fails, although spuriously.
  sameU(u1)(t1, qT1) //Succeeds! Yeah!

  sameU2(t1, qBadT1) //Does not fail, even though qBadT1 was upcast. Is this good?
  sameU2(t1, qT1)

  sameU2(t1, qBadT2) //Does not fail either - doh!
}

