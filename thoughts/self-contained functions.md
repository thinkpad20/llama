## Self-contained functions

These are a very interesting consequence of anonymous traits. Consider a module consisting solely of:

```
with foo: a -> (b, a), (+!): a -> (b, b) -> Int
bar: (x: a): Int =
  y, x' = foo x
  x' +! (y, y)
```

The type of `bar` checks out, even though `foo` and `+!` are undefined. We assume that there are functions of the types given, so `bar` would only fail if it made an incorrect conclusion with those assumptions (e.g., reporting that it returns a `Str`). More precisely, we assume that the value set `foo` will contain some function which unifies with `a -> (b, a)` and similarly for `+!` (Which means that going by existing notation, the symbol `~` might be preferable to `:`). We won't be able to *call* `bar` until there are such functions defined, so it is safe to write this function and know we won't run into an undefined error. Also, note the type `b` is contained entirely inside of `bar`, defined by the type signatures of `foo` and `+!`.

In this way they are similar to Haskell type classes, but once again, there's no need to declare the class explicitly. That said, we can be explicit if we want to.

```
type Ordering = GT; LT; EQ
trait WeirdPlus a b = (+!+): a -> b -> (a, Int, b)
trait Compare a b = compare: a -> b -> Ordering

with Compare a b
x:a > y:b : Bool = 
  #|Returns True if its first argument is greater than its second.|#
  x.compare y == GT

with WeirdPlus a a, Compare a Int
blix (x: a): a =
  x', i, x'' = x +!+ x
  if x' > i then x' else x''
```

What if there *is* an existing function? In that case, the function captures the value:

```
foo: a -> (Float, a) = a, 1.234

with (+!): a -> (Float, Float) -> Int
bar: (x: a): Int =
  y, x' = foo x
  x' +! (y, y)
```

This time `bar` captures the previously declared `foo`, and will not work with any arbitrary `b` type as before (this is reflected in the type signature). It still *could* declare a more general type for `foo` (or indeed the same type that `foo` already has), in which case it simply wouldn't matter whether `foo` were already defined.

What if we wanted to put constraints on `b`, specifically?

```
with foo: a -> (b, a), (+!): a -> (b, b) -> Int, b in {Int, Float}
bar: (x: a): Int =
  y, x' = foo x
  x' +! (y * 6, y^2 - 3)
```

Now this will work as in the first example, provided that the `b` found in in the signatures of `foo` and `+!` is either an `Int` or a `Float`. Having that, we can now use the functions `*`, `^` and `-` on the `b` value, even though we don't know what type it is yet.

Note: this latter feature would seem to be exceedingly difficult without runtime reflection (and indeed, it invites runtime reflection by the programmer, if a function can take one of a finite set of types). Need to thinking about this.
