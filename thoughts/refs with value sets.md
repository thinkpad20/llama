
# Reference cells with multiple values cause headaches.

`foo` is a constant and cannot be changed within this scope.

```
foo = 1: Int
```

`foo` is a singleton value set, containing only `1::Int` (i.e. the constant `1` as an `Int`. So `foo:Int`.

`bar` grabs a copy of `foo`.

```
bar = ref foo
```

`bar` is also a singleton value set. This is because although `ref` is lower-case and not technically a type constructor (because `ref` does some behind-the-scenes juju), it acts like a type constructor (e.g. `Just`, `Left`, etc), so it is forced to have a single value as all type constructors do. So `bar: Ref Int`.

```
println "#{foo}, #{bar!}" 
#=> 1, 1
```

We can modify `bar`, but it has no effect on `foo`.

```
bar += 1
println "#{foo}, #{bar!}" 
#=> 1, 2
```

So what happens if `foo` has more than one possible values?

```
foo = 1
```

Now `foo` has more than one possible type. Let's assume numeric literals can either be `Int` or `Float`. So `foo :~ {Int, Float}`. Then we can access either aspect of `foo`, just as we can do so with `1`:

```
println (foo/2 : Int)   # => 0
println (foo/2 : Float) # => 0.5
```

(Note: technically the above would be ambiguous, because `/` can take floats or ints, and produce floats or ints, so it could reach `Int` via `Int -> Int -> Int`, `Float -> Int -> Int`, `Int -> Float -> Int`, etc, and similarly for floats. This is a bit of nasty business which might require a reimagining of the polymorphic numerical constant idea).

Now let's grab a ref'd copy of `foo`:

```
bar = ref foo
```

So what's the type of `bar` now? Well, it's also a value set. It's type is: `bar :~ {Ref Int, Ref Float}`. It should be able to act like either of these.

What happens if we manipulate it?

```
bar *= 2.5
```

Well, what are the types of `*=`? There are probably several. Let's assume they are only these:

```
(*=) :~ {Ref Int -> Int -> Int, Ref Float -> Float -> Float,
         Ref Float -> Int -> Float, Ref Int -> Float -> Float}
```

So, which of these unify with what we have? Decimal literals must be floats, so `2.5` is only a float. So the only thing that could result is:

```
bar *= 2.5 :~ {Float}
```

So `bar` can be either a float, but its path there is ambiguous (it could take the `Ref Int -> Float -> Float` route or the `Ref Float -> Float -> Float` route). So we can use a specific cast to clear this up:

```
bar:Ref(Float) *= 2.5
println bar! # => 2.5
```

This forces us to confront an important question: did `bar`'s type just change to be `Ref Float`? Because a step ago it was `{Ref Int, Ref Float}`. This sounds very problematic, but it's not the only place where that's an important question. Consider:

```
foo: Int = 1
# OK, foo :~ {Int}.
foo: Str = 'hello'
# OK, foo :~ {Int, Str}
```

In the above, we have another example of a "changing" type. Now we could say that `foo`'s type was always `{Int, Str}`; that the order of the definitions doesn't matter. That seems good, but as it stands, the order of definitions matters a lot. For example:

```
foo: Int = 1
println foo:Str
foo: Str = 'hello'
```

If we're evaluating statements in order, that's a big problem. We could say, "we evaluate all definitions before we evaluate side-effects." Or we could go by a dependency model: the second statement depends on `foo` as a string, so we need to evaluate that first. But then how about:

```
s: MutStr = 'hello'
foo: Int = 1
println foo:Str
s += ', world'
foo: Str = s!
```

In this example, we create a mutable string, modify it, and then read it into the (immutable) definition of `foo` as a string. Clearly, order of these statements matters, because it determines whether `foo:Str` is "hello" or "hello, world". Going by "definitions before side-effects", it would be "hello", before the effect of `+= ', world'`. Going by the dependency model, it would be "hello, world", because foo depends on s, and we would resolve all effects first. Neither one is represented well in the code, because either we print the wrong thing, or we assign the wrong thing. What the *code* suggests is that we have an error in the print statement, because `foo` as a string has not yet been defined. But then this brings back the original question, which is, did foo's *type* change? Is this necessarily effectful?

It would seem, provisionally, that we are indeed suggesting that types can change. But even conceding that point, it seems crazy to suggest that types can *lose* information, i.e. that you'd go from having `foo :~ {Int, Str}` to just `foo: Int`. 

But getting back to the `ref` example, that's exactly what seems to be happening. We modified what's in the reference cell from something which is either an int or a float, to something which is necessarily only a float.

Although this is clearly requires a lot of thought, what this strongly suggests for the time being is that `ref`s require that their inputs are only of a singleton type set, because otherwise, we would need to be able to support all sorts of different representations simultaneously in memory.

```
foo  = 1
bar  = foo:Int
baz1 = ref bar      # OK
baz2 = ref foo:Int  # OK
baz3 = ref 1:Float  # OK
baz4 = ref 1        # Error: attempt to create a reference cell with a value 
                    # set of more than one element. Use a specific cast.
baz5 = ref foo      # Error: attempt to create a reference cell with a value 
                    # set of more than one element. Use a specific cast.
```

Note that this isn't a problem with immutable data.

```
baz = Just foo
```

Here, `baz` has a type set `Maybe Int, Maybe Float`. No matter what operations we do to `baz`, it will retain the necessary information to store these representations.
