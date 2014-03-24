When we encounter `func x` in a vacuum, we don't know anything about the type of `func` except that it must be able to accept as an argument whatever type `x` might be. Similarly, we don't know anything about `x` except that it must be one of the types that `func` accepts. Similarly to polymorphic/monomorphic type concepts, `x` must have a single monomorphic type: within the scope that `x` occurs, it can have only one type.

So we can come up with the following steps when applying `func` to `x`:

```
x: t0
func: t1
Constraint: t0 `IsInRange` t1
func x: t2 where t2 is fresh
Constraint: t2 `IsResultOf` (t1, t0)
```

In other words:

* We assume `x` has been given some type `t0`, and `func` has been given a type `t1` (this means they must already exist in scope, either via `let` or as a lambda variable).
* Since `func` is being applied to `x`, we know that `t0` must be able to be unified with at least one of the types in the range of `t1`.
* We create a fresh type variable `t2` for the type of `func x`. We add a constraint that `t2` is, or can be unified with, the result of applying `t1` to `t0`.

How about `x => foo x`?

Assume `foo` has a type `t0`. We create a scope with `x` assigned to type `t1`. We go through the above steps for applying `foo` to `x`. Then we determine that

```
Starting assumptions: {foo: t0}
x: t1, fresh
Constraint: t1 `IsInRange` t0
foo x: t2, fresh
Constraint: t2 `IsResultOf` (t0, t1)
x => foo x: t3
Constraint: for all t in t1, Result (t3, t) = Result(t0, t)
```

Blargh. Needs moar work. Implementing this imperatively might be better?

Others:
* `y => {z = foo x; z y}`
