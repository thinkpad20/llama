We should have a distinction between named and unnamed functions. Also distinction between exported and non-exported functions. For example:

```llama
module A
some_function (i: Int) = float i * 5.5
```

```llama
module B
import A\some_function

some_function (s: Str) = reverse s

println 14.some_function      #=> 77.0
println 'hello'.some_function #=> olleh
```

We have overlap, but that's fine because:

- We declared `some_function` with a name.
- We declared it with a type. We have two different definitions based on its first argument.

By contrast:

```
func a = a * 2
```

This `func` does not get stored on any type, because we didn't declare it to do so. We can still use it:

```
println func 123 #=> 246
println 30.func #=> 60
```

But we can't overload it:

```
func b = reverse b # Error: There is already a function `func` with no declared type
```

We *can*, however, overload it with a type

```
func (s: Str) = reverse s
```

Then what if we had something like:

```
foo = map func [1, 'hello', 2, 'world']
#=> foo = [2, 'olleh', 4, 'dlrow']
```

This would work because at runtime we would detect that `func` is overloaded, and choose the appropriate instance. 

**However**: can we still infer the type of `foo`?

```
> a = [1, 'hello', 2, 'world']
> :t a
a: [?]
> b = map func a
> :t b
b: [?]
```

So `[?]` isn't really very helpful, although I suppose it's more information than `?` alone. What if we could be just a little smarter?

```
> a = [1, 'hello', 2, 'world']
> :t a
a: [{Int, Str}]
```

Then what about applying `func`?

```
> :t func
func : {Str -> Str, ? -> ?}
> b = map func a
> :t b
b: [{Str, ?}]
```

Really though, `b` is just `[?]`, because "`Str` or any" isn't any more informative than just "any".

An interesting question is what to do about type variables. For example:

```
id (x: a) = x
```

In Hindley-Milner land, `id` has the type `∀a. a -> a`. In a naïve dynamic typing scheme, `id` simply has the type `? -> ?`, which is just "some function." But we can prove that given an input of any type, `id` will return *that* type. We shouldn't need to lose that information. Similarly:

```
singleton (x: a) = [a]
```

In this example, `singleton: ∀a. a -> [a]`. The point is that we want a distinction between *type variables* and *the dynamic type*. This has implications on member functions; for example:

```
foo x = x + 1
```

This doesn't say what type *x* is, so we assume it's the dynamic type. Therefore, it doesn't get added to any type's function dictionary. It's only at runtime that we could find out there's an error in calling `func` on, say, a vector. On the other hand:

```
foo (x: a) = x + 1
```

This *would* get added to all type's dictionaries, except that because we've now typed it, we can see that it's an error: There is no function `+` which exists for the "general" type (distinct from the dynamic type!). We probably instead want to make `foo` any numeric type:

```
foo (x: some Numeric) = x + 1
```

Now, we can add `foo` to every instance of the `Numeric` trait (really, it's stored on the trait itself; this lets it be inherited by new instances as well).

We can contrast this with lambda functions, which have no name and don't add anything to their types:

```
bloop = map (x => x + 1) [1,2,3]
```
