```
trait Convert a b = convert: a -> b
trait TwoWayConvert a b = Convert a b; Convert b a

module A exports do_thing
  convert: Str -> Int = ...
  convert: Int -> Str = ...
  # inferred: do_thing: {Str -> Int, Int -> Str}
  do_thing = x -> convert x
end module
```

OK, so it seems that we're leaning towards inferring more explicitly: as in, if something isn't defined, then we'd throw an error there.

This would be an error:

foo = x + y

Because there is no `x`, `y`, or `+`!

foo: {(+): a -> b -> c, x: a, y: b} => c

OK so it looks like we have two ways of doing polymorphism: by referencing actual value sets (e.g., `foo` when foo has been defined in some way), or by referencing "hypothetical" value sets (aka type classes), saying that "given that there exists some foo satisfying these constraints"...

```
# actual value set
x: Str = 'hello'
x: Int = 23
x: Float = 23.45
x: Int -> Str = i -> show i + '!!'

# hypothetical value set, doesn't grab actual values of x or y until called
context x: {Str, Int}, y: Int -> Str
do_thing _ = if y x == x then throw Exception('Uh oh!') else println 'ok!'

do_thing x # prints 'ok!'
```

A few types of constraints: `x has Int aspect`. `x has Int aspect and Str aspect`. `there exists a value set f with an Int -> Str aspect`. All of these things can be boiled down to assumptions that some value set will be present with values of some type. For example:

```
foo = x + y
```

Assumes that there exists a `+`, an `x` and a `y` such that there are one or more types in `x` that are compatible with the argument types of `+`, etc. This can be checked at compile time.
