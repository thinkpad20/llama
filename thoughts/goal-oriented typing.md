```
trait Convert a b = convert: a -> b
trait TwoWayConvert a b = Convert a b, Convert b a
```

A trait is an assertion that a set of functions with given names and types exist. But the type variables in traits are generalized: they don't refer to any specific types yet. On the other hand, saying: 

```
foo: a -> Str with {Convert a Int} = .convert.~replicate '!'
```

In this case, `a` is fixed (within the scope of `foo`). `foo` will work on any `a` where there exists a function named `convert` with type `a -> Str`.

When we type this, we need to have `convert : a -> Int` in the scope before we do inferrence (we might not "need" this, but at least to start). The functions `~_` and `replicate`, on the other hand, must already exist and be defined.

The idea I had recently was one of a "goal type": instead of just doing inferrence, we have some *goal* that we're trying to reach, given by the user. The numeral `1` could refer to any type for which there's a `fromint` function:

```
λ> :typeof 1
1 : a with {fromint: Int -> a}
```

But when we type this, we set the *goal* that we're looking for an `Int`. 

```
x: Int = 1
```

Then we could use this goal to guide inferrence...
