## Using some overloaded functions in Llama

Let's define `+` with the type `[a] -> a -> a`. We already have a function of this type, which is what we want: `append`. Note that with the inferrence rules we defined, `append` will be coerced into the type that we want, so we will get the implementation we expect.

```
(+): [a] -> a -> a = append
```

Now we can define `+=` for a `Ref` vector and a value, as an update operation. Note that this is *not* the same as for an `Array` and a value: there, the `+=` operation would be destructive on the array itself, not just the ref cell. We can be as fine-grained as we want.

```
(+=): {+: a -> b -> a} => Ref a -> b -> a =
  r -> x -> r := !r + x
```

So we read what's in the ref cell, add `x` to it, and update the ref cell to contain that new value.

Now we can define `replicate`:

```
replicate: (Int, a) -> [a] =
  (n, _) if n < 0 -> throw Error('Negative argument')
  (n, x) -> result! after
    result = ref []
    for _ in range n do result += x
```

In fact, we could generalize `replicate`, for any immutable structure:

```
replicate: {(+): c -> a -> c, Empty c} => (Int, a) -> c =
  (n, _) if n < 0 -> throw Error('Negative argument')
  (n, x) -> result! after
    result = ref []
    for _ in range n do result += x
```

Now we could use `replicate` with sets, vectors, lists, etc. Although for any of these there might be more efficient versions, so we could specialize. For example, `replicate` for vectors is already specialized.

We could also use a mutable array for this:

```
# `mut_append` is a destructive array-append library function.
(+=): {+: a -> b -> a} => Array a -> a -> Array a = mut_append

replicate: (Int, a) -> Array a = 
  (n, _) if n < 0 -> throw Error('Negative argument')
  (n, x) -> result after
    result = []
    for _ in range n do result += x
```

All we had to do was provide a satisfactory update operator, and we no longer had to read the output.
