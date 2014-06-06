Consider these definitions of foo:

foo : Int = 1
foo : Str = 'hello'
foo : Int -> Int = (+ 1)
foo : Str -> Int = length

Then *types*(foo) = {Int, Str, Int -> Int, Str -> Int}

Now consider the expression `foo foo : Int`. Certainly, this should be possible: we can use the various values of `foo` to produce an `Int` from applying `foo` to `foo`. However, it's ambiguous, because there is more than one way to do it:

1) 
  foo foo : Int = 2
  using (foo : Int -> Int) (foo : Int) = (+ 1) 1 = 2
2)
  foo foo : Int = 5
  using (foo : Str -> Int) (foo : Str) = length 'hello' = 5

We would need specific type annotations to resolve this.
  foo (foo : Int) = 2, unambiguously.
  foo (foo : Str) = 5, unambiguously.

The operator `:` has the following property:
  For any expression `e` and any type `t`, the expression `e : t` either fails, or evaluates to a value of type `t`.

So we can do things like:
```
# Definitions of foo
foo : Int = 1
foo : Str = 'hello'
foo (Just i:Int) = "it's just #{i}!"

# the type set of foo is {Int, Str, Maybe Int -> Str}.

# Using foo
println foo:Int # prints "1"
println foo:Str # prints "hello"
println foo(foo(Just 1)) # prints "it's just 1!"
```

Note that in the third definition, we didn't need to type `Just i` as a `Maybe Int`. This is because constructors are constants, not variables, and the expression `Just a` has the type `Maybe t` for any `a:t`, and only that type. In fact, constructors are "more constant" than numbers or string literals, because numbers can be either ints or floats of various sizes, and string literals can be list strings, bytestrings, unicode, etc.

The ability to restrict evaluation to a single type lets us:
* make the argument types of functions unambiguous.
* force strict evaluation.

One interesting thing here: there is a semantic difference between
```
foo = 1 : Int
```
and
```
foo : Int = 1
```

The first one assigns `foo` to the integer 1 (and `foo` must not already be defined in the same scope). The second one *adds* the definition of `1` to the *set* of values denoted by `foo`. If `foo` does not exist in scope, it creates a new set. Note that there isn't a need to type the right side (`foo: Int = 1 : Int`, because this coercion will happen automatically). A way to see it is that in the first example, `foo` is of type `Int`, while in the second, `foo` is of type `TypeSet {Int}`.

How about for functions?

```
bar i = i + 1
bar : Int -> Int = i -> i + 1
```

In the first case, `bar` is the value `i -> i + 1`, while in the second, `bar` is the a value set, where the `Int -> Int` version of `bar` is `i -> i + 1`. Note that in the first case, `bar` is polymorphic; it can accept Floats, Ints, or whatever else `+` is defined for, and return multiple values accordingly. In the second case, `bar` can only accept Ints and return Ints. If we wanted to make the first version of `bar` specific, we could write it

```
bar = ((i -> i + 1) : Int -> Int)
```

And if we wanted to make the second one polymorphic, we could write it

```
bar : {(+): i -> i -> k, fromint: Int -> i} => i -> k = i + 1
```

Or: 
```
trait Add i j k = (+): i -> j -> k
trait FromInt i = fromint: Int -> i
bar : {Add i j k, FromInt i} => i -> k = i + 1
```
