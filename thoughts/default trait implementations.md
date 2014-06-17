What happens when we have multiple

```
trait Add a b c = (+): a -> b -> c
(+): Int -> Int -> Int = ...
(+): Int -> Float -> Float = ...
(+): Float -> Int -> Float = ...
(+): Float -> Float -> Float = ...
```

These are all prefix-unique, so if this were all we had, we could unambiguously guess:

```
x: Int = 1
y: Int = 2
println x+y
```

Typing the `x+y`, we'd get `{Add Int Int a}. a`, and the only instance we have matching `Int Int _` is `Int Int Int`, so we'd use that.

This seems like a pretty reasonable way to do things, and would eliminate a lot of cases of type ambiguity. But what if we had real ambiguity? What if we had another definition of `(+)`, with type `Int -> Int -> Float`? This probably isn't a great idea, but we could do it and if we did, we would have a problem, because `x+y` would only type to `{Add Int Int a}. a`, and we'd have two choices for the final `a` (`Int` or `Float`).

But obviously, most of the time we'd only want an `Int`. So we could make a rule like:

```
default Add Int Int _ = Add Int Int Int
```

Which would say "if there is a trait ambiguity and the assertion satisfies the pattern `Add Int Int (some type variable)`, then treat it as `Add Int Int Int`." Of course, the assertion on the right would have to be true (e.g. there would have to be an instance of `Add Int Int Int`, which there is).

Such a thing, if it were to work, would likely eliminate the need for functional dependencies in all but the most extreme cases.

It's worth wondering what kind of effect trait aliases would have on this. If a trait aliases another trait, could we give the trait a default which might cause a conflict? For argument's sake:

```
trait Foo a b = foo: a -> b
trait Bar a b = Foo a b

foo: Int -> Str = show
foo: Int -> Int = (+ 10)

default Foo Int _ = Foo Int Str
default Bar Int _ = Bar Int Int

println foo(3)
```

Here it would seem that we would get output `"3"`, because the type of `foo` references the trait `Foo`, not `Bar`. If we wanted `Bar`'s default, we would need to *explicitly* reference `Bar`:

```
f: {Bar a b, AddSelf a} => a -> b = x -> foo (x + x)
println f(3)
```

This time we would get output `13`, because `f` specifically references `Bar`, so when the ambiguity occurred, `Bar`'s default would be chosen.

But actually, would we still get `"3"`? Because `Bar` is only a *reference* to the trait `Foo`. `Bar` gives rules for how to satisfy itself ("tuple (`a`, `b`) must be in the set `Foo`") and nothing else. So, a better option might be to disallow `default`s in traits which reference other traits.
