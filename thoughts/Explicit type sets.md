Explicit type sets:

```
x: Int = 1
x: Str = 'hello'
x: Int -> Str = int_to_str
x (x:a): [a] = [x, x, x]
...
```
Then `x` has the explicit types given out by its value set; i.e. `x: {Int, Str, Int -> Str, forall a. a -> [a], ...}`.

Inexplicit type sets:

```
with (<&>): a -> a -> Int
foo (x:a): Int = x <&> x
```

In the first example, we are hypothesizing that the value set denoted by `<&>` contains a value of type `a -> a -> Int` for some a, and assuming that hypothesis holds, we show that we can add a value of type `a -> Int` to the value set of `foo`.

Whatever `a` gets passed into `foo`, we are assuming that there exists (exactly one) value in `<&>` of type `a -> a -> Int`. This value could be exactly the same as `a`, for example, we could have `(<&>): Str -> Str -> Int`, and `a` is a `Str`. Or it could be polymorphic, like `(<&>): forall t. t -> t -> Int`, in which case `a` could be anything. Effectively, the argument types available to `foo` are constrained by those available to `<&>`.

Either way, within the scope of `foo`, we don't know what type set `<&>` refers to. But let's say that `a` has been quantified with the variable `t` (just to separate the names). That is, we're inside the scope of `foo`, and `foo`'s argument type has been given the name `t`. Then the type of `<&>` is fixed within this scope: it must be of type `t -> t -> Int`. So it's a singleton type set.

The type of `foo`, on the other hand, represents a set of types: it's the set `{a -> Int | exists (<&>): a -> a -> Int}`. This type set is not explicit; we're not listing all of the types. We're instead specifying rules to determine if a given type exists in this type set.

So what we have so far is that a type set *S* can be seen as a set of rules to determine, given a particular type *t*, if *S* is compatible with that type (i.e. that there is a type in the set which can be unified with *t*). We can write a function `(~): Type -> TypeSet -> Bool` which can make this determination. Another way of looking at it is that a type set is a predicate on types.

So how about traits?

```
trait Add a b c = (+): a -> b -> c
```

Here the type set of `+` is also implicit, but its definition appears to be circular: it's the set of types `a -> b -> c` such that `a -> b -> c` is in the type set of `+`. But `+` *refers to* a specific set (initially empty), which is populated by making specific definitions. Thus `+` is an implicit type set in that it refers to, but does not enumerate, a specific set of types. If we define `(+): Nat -> Nat -> Nat`, `(+): Int -> Int -> Int`, etc, then those become the types referred to by `+`. 

Then declaring a trait has the effect of a forward declaration. It introduces the variable `+` into the environment, referring to some initially empty set of values. It lets us use the variable `+` even though it hasn't been defined.

Then what we can say about the type of `<&>` is that the variable `a` has already been introduced to the type environment

This means that within the scope of `foo`, `<&>` has the implicit type set `{forall a. a -> a -> Int | exists t in types(<&>) ~ a -> a -> Int}

In the second example, we are creating a hypothetical value set `+`, which contains (possibly along with other things) a value of type `a -> b -> c` for some `a`, `b`, and `c`.

This implicitly creates a 
