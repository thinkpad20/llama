## An approach to subtyping:

Subtyping introduces several issues. Here's an example:

```
type T = A; B; C

foo: T -> T = t -> println t; t

assert foo(A) == A # succeeds, prints A as side-effect
```

That's all well and good, but say that I subtyped `T`?

```
type T2 <: T =
  X <: A
  Y <: B
  Z <: A
```

If `T2` is a subtype of `T`, we should be able to use it whenever we have a `T`. So what does that do with `foo`?

```
assert foo(X) == X # Type error: no instance of `==` for `T, T2`
```

Wait, what? How did we put in a `T2` and get back a `T`? Well, `foo` only knows about `T`s, not `T2`s. This produces a weird situation where the type of `foo` makes it look like a mapping from `T`s to `T`s, and each `T2` is a `T`, so it seems like it should map from `T2`s to `T2`s, but it doesn't.

Here's a possible approach to this. Create a new type of assertion, of the form `t <: u`, which means `t` is a subtype of `u`. Like assertions of traits, this is a relation on types. Then we can do something like this:

```
foo: {t <: T}. t -> t = x -> println x; x
assert foo(A) == A # succeeds, prints A as side-effect
assert foo(X) == X # succeeds, prints X as side-effect
```

This isn't really that helpful, unless we can peer into the `T` part of the type. Perhaps we could do something like `as` as a meta-function to do this:

```
bar: {t <: T}. t -> (t, T) = case t as T of
  A -> throw Exception("We can't deal with A's for some reason!")
  _ -> (t, C)
```

This function is pointless but would give us the behavior we want: namely, the ability to access the parent values from a child value. This is pretty rough, though, and ultimately might not be worth doing. It also has the negative side of requiring special type signatures which would make it pretty annoying in practice. But it might lead somewhere as an idea.
