Another way to think of it is that the function

```
bar: {foo: Int -> Int} => Str -> Int = .length.foo
```

Creates an internal variable `foo` which represents an empty type set; then when `bar` is used in a module (or some other context), this variable is supplied by that context. So for example:

```
bar: {foo: Int -> Int} => Str -> Int = .length.foo
println 'hello'.bar         # ===> Error: context contains no foo: Int -> Int
foo: Int -> Int = (+ 1)
println 'hello'.bar         # ===> 6
```

This is very similar to the type class implementation in Haskell, but we can do it implicitly. If you have a function `bar: {foo: a -> Int} (x:a): Float = x.foo.convert`, what this is saying is:
  * find a value set named `foo` in the context of where this function is run
  * get the value of it which has the type `Int -> Int`
  * apply it to the argument
  * apply `convert` (which has type `Int -> Float` to the result
  * return it.

Note the difference between `foo` and `convert`. Because `foo` shows up in the context of `bar`, no capture is performed. `foo` is waiting to be filled in when it is run (although this doesn't need to *actually* happen at runtime). On the other hand, `convert` is not in the context of `bar`; it's in some earlier context, which means that whatever it was set in there (which itself might be polymorphic) is *captured* by `bar`. This also means that it *must be in scope*, while `foo` need not be in scope at all.

A question, then, is can we infer what's a trait and what's not? A simple way is to say "if we've seen the variable, then it's a capture; otherwise it's a trait variable". For example:

```
type Nat = Z; S Nat
(+): Nat -> Nat -> Nat = 
  Z -> n -> n
  S n -> m -> S (n + m)
```

We didn't need any captures for those, because all of the required variables were in scope in all definitions. Moving on:

```
do_thing1 (x, y, z) = x + y + z
```

Once again, we can infer everything present, so we don't declare any trait functions: we infer the *most specific type*, which is `do_thing: (Nat, Nat, Nat) -> Nat`. Is it possible to make `do_thing` more general? Yes it is: the most general case is a much more verbose type with three callables and many types, but we opt for conservativity which also lets us catch errors more quickly (e.g. if we wrote `x + 'abc'`). So, value sets with *known contents* are inferred using only what is in those value sets. So that, for example, we would know that `x + 'abc'` is an error, because there is no value with type `Int -> Str -> a` in `+`'s value set. Similarly if we wrote `x y + z` we would error because there is no `Nat -> Nat -> Nat` in `@call`'s type set.

So then, where we do the opposite and go general is when there are *unknown* identifiers. What if we had this?

```
do_thing2 (x, y) = x * y
```

In this case, we don't know what the type set of `*` is. We infer that it must be some trait function, and we create a type accordingly:

```
do_thing2: {(*): a -> b -> c} => (a, b) -> c
```

If we used it in conjunction with `+`, we could get a bit more specific:

```
do_thing2 (x, y, z) = x+y * z
```

Here we can infer from the `+` that  `x` and `y` are of type `Nat`, so we can say:

```
do_thing2: {(*): Nat -> a -> b} => (Nat, Nat, a) -> b
```

It seems a little dangerous not to report unknown identifiers, but the thing is, we won't be able to *use* `do_thing2` until we have a satisfying context for it; that is, until there *is* some function `*` with a type that unifies with `Nat -> a -> b`, and that `z` is correctly typed, etc.

So our unification rules work to support that: when we apply `a` to `b`, we require that the context support whatever context `a` and `b` require. This means that we can use `*` all we want within the context of `do_thing2`, but we can't apply `do_thing2` to anything until we define `*`.

The interesting thing is that `+` is captured by `do_thing2`, but `*` is not. Even if we later expand the definition of `+` with more types, the version called in `do_thing2` (and all other functions so far) will be a snapshot of the current one. This is in line with the closures concept and helps ensure that behavior doesn't change arbitrarily.

```
foo = bar 4 after
  bar x = y -> x * 3 + y
```

In the above, we get an error when `bar` is applied to `4` because we can't apply `bar` without knowing what `*`'s type is. It would probably be better to *delay* the application until we need to evaluate the right-hand side, so that this would be legal, and we would get the type that

```
foo: {(*): a -> b -> Nat, IntLit a, IntLit b}: a -> Nat -> Nat
```

Now an interesting question; what if we expand the definition of `+`?

```
(+): Str -> Str -> Str = concat
```

Now what happens when we use `+` without a type signature?

```
baz x = x + x
```

So now we have two choices for `+`. We could throw an error, saying it's ambiguous, or we could include all current `+`s in scope:

```
baz: {Nat -> Nat, Str -> Str}
```

This would seem to scale precariously: let's say we have 5 types for `+`, and we have this function:

```
qux x = x + x + x + x + x + x
```

Then any of the `+`s could be any of the types, and `x` could have aspects for any of those types, so any combination of them is conceivable, and we end up recording `5*5*5*5*5 = 3125` types for this function. For an operator with, say many instances like Monoid, and using it many times in a function, we'd blow up our memory. So this is not a good way to go. We need to think more about this. Another potential option is to use an implicit trait when the type class is ambiguous:

```
baz: {(+): a -> a -> b}: a -> b
```

Because effectively, what `+` is becoming is a trait function then. But we *do* want to capture the `+`s defined in the current scope...
