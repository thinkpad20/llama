Representing an infinite type?

This probably isn't that useful, but it's at least fun to think about this:

```
with Show a
print_curry: a -> @typeof print_curry = println x; print_curry
```

That would mean we'd need some way to defer the checking of `print_curry`. A similar function in Haskell fails to type check, unsurprisingly:

```
Prelude> let foo x = print x >> return foo

<interactive>:8:31:
    Occurs check: cannot construct the infinite type: b ~ a -> IO b
    Relevant bindings include
      x :: a (bound at <interactive>:8:9)
      foo :: a -> IO b (bound at <interactive>:8:5)
    In the first argument of ‘return’, namely ‘foo’
    In the second argument of ‘(>>)’, namely ‘return foo’
```

Just a fun thing to wonder about. How about another function which fails with infinite type error?

```
Prelude> let foo x = x x

<interactive>:9:15:
    Occurs check: cannot construct the infinite type: t1 ~ t1 -> t
    Relevant bindings include
      x :: t1 -> t (bound at <interactive>:9:9)
      foo :: (t1 -> t) -> t (bound at <interactive>:9:5)
    In the first argument of ‘x’, namely ‘x’
    In the expression: x x
```

The issue here is the monomorphism idea, that `x` can have only one meaning within the scope of `foo`. Arguably, this is not the case in Llama (although we seem to be heading in that direction for argument variables).

```
foo: a -> a with {Callable a a a} x = x x
a: Int = 1
a: Int -> Int = (+ 1)
assert (foo a == 2)
```

I think we probably want to avoid this though (assuming it's sound, which it may well not be), because it would suggest that a runtime-reflection based system is required, and we don't want to make that a requirement (at least initially).
