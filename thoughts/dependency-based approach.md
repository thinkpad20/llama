OK so another idea I had on the bus...

Dependency trees! Yeah I know we tried that and it kind failed, but this time we have new knowledge!

let's say we've already encountered `foo`, so that it has been assigned a type variable `a`. Then at
some point we encounter the expression `x => foo x`. 

* We create a new type variable for `x`, call it `b`. It has a set of possible types `setOf(b)` that, initially, is unbounded.
* Since `foo` is applied to `x`, we create the restriction that `b` is in `a`'s domain; that is, `b` must be a type acceptable to the function type `a` (`foo`'s type).
* As to their result, we create a new type `c`. We define `c` as "the result of `a` applied to `b`", and don't express it any further: it depends on those two types and nothing else.
* Finally, for the type of `x => foo x`, we have determined that the right side is `c`, and we know the left side is `b`, but `b` could have multiple (or infinite) possibilities, so we can't determine the type yet. Instead we say `x => foo x: d` and we define `d` as `{t -> c(t) for t in setOf(b)}` (which isn't solved yet).


We end up constructing a dependency tree:

```
d == {t -> c(t) for t in setOf(b)}
c == u `AppliedTo` b
setOf(b) `IsSubset` domain(u)
```

So in this case, knowing `u` tells us everything we need:

```
u == {t0 -> t1, t2 -> t3, ...}
==> domain(u) == {t0, t2, ...}
==> setOf(b) == {t0, t2, ...}
==> c(t) == case t of t0 => t1 | t2 => t3 | ...
==> d = {t0 -> t1, t2 -> t3, ...}
```

Which is the correct solution.
