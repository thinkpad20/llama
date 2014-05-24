```
> vecs = [[1,2,3], [4,5,6], [7,8,9]]
[[1,2,3], [4,5,6], [7,8,9]]
> :t vecs
vecs : [[Num]]
> arrs = map array vecs
[[1,2,3], [4,5,6], [7,8,9]]
> :t arrs
arrs : [Array Num]
```

```
trait Array with 
  array      : Self a -> Array a
  from_array : Array a -> Self a
```

```
hash : Set a -> HashSet a
hash : Map a b -> HashMap a b
```

We can see here that the function `hash` has two perfectly good signatures, which are incompatible with each other. What can we do about this? Should we just leave both in? How could we type check?

Since we're using gradual typing, we can just use the dynamic type:

```
> foo x = (hash x).add (1, 2)
> :t foo
foo : _ -> _
```

But what if we do want to express something meaningful in the type signature? We could use a trait, but then we'd be declaring a specific type for `hash`. How about an anonymous trait? A trait defines a set of types which satisfy some interface.

```
to_hash (x : _ of {hash : Self -> a}): a = hash x
```

Here we're specifying that the argument is of a type in the set where there exists a function `hash`, which takes `Self` and produces an `a`. We've basically lost all type information after this, because we don't know what `a` is, so we can't really do much with it (type checking wise. We could give it the dynamic type and go crazy, but with type variables we need to produce an `a` which constricts our options).

But we could constrain it a bit; for example, the set of types whose `hash` method produces a `HashMap`:
```
get (m: _ of {hash: Self a b -> HashMap a b}) (key: a): Maybe b = 
  try key |> m.hash |> Just
  catch KeyError do Nothing
```

What we seem to be seeing is that trait functions might be too restrictive. For example, both sets and maps have to_list and from_list methods, but their types are not the same:

```
to_list : Set a -> List a
to_list : Map a b -> List (a, b)
```

Nevertheless, it would be nice to be able to generalize objects of either kind:

```
show_pairs (m: _ of {to_list: Self a b -> List (a, b)}): Str =
  go (a, b) = '#{a} => #{b}'
  '{#{join(m.to_list.each go, ', ')}}'

show_single (s: _ of {to_list: Self a -> List a}): Str =
  '{#{join(m.to_list.each show, ', ')}}'

s  = {1, 2, 3}
v  = [2, 3, 4]
m  = {1 => 2, 3 => 4}
hm = hash {1 => 2, 3 => 4}

assert show_single s == show_single v
assert show_pairs m == show_pairs hm
```

We can even imagine having two different traits which use the same function name; differing in the *type* that function has:

```
trait ToList with to_list : Self a -> List a
trait ToListPairs with to_list : Self a b -> List (a, b)
```

Then the trait functions would act somewhat like a multifunction:

```
> :t to_list
to_list : t a -> List a | t of ToList
to_list : t a b -> List (a, b) | t of ToListPairs
> :t to_list [1, 2, 3]
to_list [1, 2, 3] : List Num
```

We could go crazy:

```
single_to_double : t a -> u a b -> [(a, b)] | t of ToList, u of ToListPairs
single_to_double a b = result after
  list1 = to_list a   # normal list
  list2 = to_list b   # list of pairs
  result = ref []
  for x, y in list2
    if list1.contains x then result .= push (x, y)
```

So basically in this system, a single name can point to one *or more* functions, indexed by type.

```
trait A with f: Int -> Self
trait B with f: Self -> Int
implement A for Str with f = show
implement B for Str with f = length
```

But this could get weird really quickly. Look at this example:

```
implement A for Int with f = id
implement B for Int with f i = ~i
```

What is the type of `f 1`? It could be `Int`, using `B for Int`. It could also be int could be `Str`, using `B for Str`. But an even weirder question is what is the *value* of `f 1`? Before, to resolve a stub we just looked up the type we needed, but we can no longer tell what the value of `f` is based on what type we want, because it's still ambiguous. Using the definition for `A`, `f 1 == 1`. But using `B`, `f 1 == -1`.

One way around this would be to allow traits to reuse names, but not allow the same function to be implemented twice for the same type. So `Int` could implement *either* `A` *or* `B`, but not both. Then the ambiguity is... gone? Just lessened? I dunno.
