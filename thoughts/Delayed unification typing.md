Delayed unification typing

We can make definitions like the following with no context whatsoever:

```
f x = result
g x y = x + y z
```

We will infer the types of these in the most general possible way, perhaps "optimistic" would be the right word:

```
f: {result: b} => a -> b
g: {(+): a -> b -> c, z: d} => a -> (d -> b) -> c
```

These typings are hidden from the user. However, when the user attempts to use one of these functions, we'll see errors. Let's say, for example, that we have the expression `f 1`. This is a top-level expression, so it would be evaluated strictly, which means that we type-check it at compile-time. At that point, we follow the rule that if an expression `e` has an assumption `A`, then `A` must be *inferrable from* (not just compatible with) the context in which it is used. `f` carries with it the assumption that there exists some `result: b`, but that is not inferrable from the context (if there were *any* variable named result, it would work, but there isn't). So we fail.

How about something that does work?

```
type Nat = Z; S Nat
(+): Nat -> Nat -> Nat =
  Z -> n -> n
  S n -> n' -> S (n + n')
```

First of all, the most general type for the definition of `+` is `{(+): Nat -> a -> Nat} => Nat -> a -> Nat` (we don't know the 'a' just from the definition, because we could conceivably be using a different `+` in the recursive call). This readily unifies with our declared type `Nat -> Nat -> Nat`, so we can move on.

Now let's make a variable that uses it:

```
f x = x + x
```

Now we initially only infer the most general type of this; that is, `{(+): a -> a -> b} => a -> b`. But let's see what happens when we use it:

```
f Z.S.S
```

From before, `f` has the assumption that `{(+): a -> a -> b}`, and we can see from its calling context that `a` is `Nat`. So we need `{(+): Nat -> Nat -> b}`. This is satisfied by the existing definition `(+): Nat -> Nat -> Nat`, so it type checks just fine, and evaluates to `Z.S.S.S.S`.
