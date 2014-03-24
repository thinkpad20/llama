When we initially create a lambda expression `x => ...`, we assume optimistically that `x` can take on any value. We create a new type variable for `x`, let's say `t` and say that `t` can be any type, i.e. `t` is a member of the set of all types. 

```
x: t
t is a member of (set t).
(set t) is the set of all types.
```

We then progressively restrict the set of types that `t` is part of be by set intersections. For example, if `foo` is of type `u` and is applied to `x`, then `t` must be in the range of `u`. 

```
foo: u
x: t
Constraint: (set t) is a subset of (domain u)
foo x: v
Constraint: v == u applied to t
```

Specifically, `v` is the result of applying `u` to `t`. However, we don't know what this type will be yet, because we don't know what `u` or `t` are yet! So, we need a way to represent this. Basically, we need a way to say that "`v` is the type that is returned when we look up `t` in `u`". We can keep this information around! We don't know what it can be without `u`, and we don't know what it *is* without both `u` *and* `t`.

So then, if we have the expression `x => foo x`, then we create a new type variable for `x` which can be any type, then we add the restriction on that type's set that it must be in the domain of `foo`'s type, then we determine that `foo x` is some type which is the result of applying `foo`'s type to `x`'s type, and lastly we say that the type of `x => foo x` is `{t: u(t) for t in (typeof x).set}`. We can create a new type with this and represent it as a contstraint, for example:
```
x => foo x : t0
Constraint: map_from (typeOf x) (typeOf )
```
Note that we have an error if `(typeof x).set` is empty.
