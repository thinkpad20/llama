Every object is descended from either Object, or Error.

```
object Foo;
object FooError <: Error;
```
Every function returns either an object, or some error type. Ex:

```
lookup: {a => b} -> a -> b
```

This signature looks like it should always return a `b`, but implicitly, it could also return an error. When we use this function, we could choose to handle the error, or not. If we don't, and it's an error, we get a `PatternMatchError` (or equivalent).

```
> dict.default_lookup key = case dict.lookup key of
.   KeyError _ => default: (dict\type 1) |
.   val => val
> my_dict = {'hello' => 'world'; 'how are' => 'you?'}
```
We can now safely call `default_lookup` as long as the key exists, or the value type of the argument implements the `Default` trait.

```
> assert my_dict.default_lookup 'hello' == 'world'
> assert my_dict.default_lookup 'poop' == ''
```

But what if the value type doesn't?

```
> my_dict' = {'hello' => 1, 'world' => '!'};
```

Now, calling `default_lookup` with a non-existent key on `my_dict'` will throw a `TraitError`, because `my_dict'` has the type `{Str => {Num, Str}}` and the type `{Num, Str}` is not an instance of the `Default` trait.

```
> my_dict'.default_lookup 'bloop'
TraitError: No instance of `Default` for `{Num, Str}`
```

We could avoid this sooner by putting a type annotation on `default_lookup`:

```
> default_lookup (dict: {_ => b of Default}) key = case dict.lookup key of
.   KeyError _ => default: b |
.   val => val
```

Now we'd error out before even attempting to evaluate the function body:

```
> my_dict'.default_lookup 'bloop'
Exception:
  In the expression `my_dict'.default_lookup 'bloop'`
    Where my_dict' = `{'hello' => 1, 'world' => '!'}`
  TypeError: 
    Can't unify `{_ => b of Default}` with `{Str => {Num, Str}}`
      Can't unify `b of Default` with `{Num, Str}`
        TraitError: No instance of `Default` for `{Num, Str}`
> my_dict'.lookup 'hello'
1
> my_dict'.lookup 'bloop'
Exception:
  In the expression `my_dict'.lookup 'bloop'`
    Where my_dict' = `{'hello' => 1, 'world' => '!'}`
  KeyError: 'bloop'
> Default for {Num, Str} = default = 0
> my_dict'.default_lookup 'bloop'
0
```

Another potential error that we'd run into is that we might call `default_lookup` with a the wrong kind of type (for example, a type with kind `*`.)

```
> 0.default_lookup 1
```

What would happen here, assuming there were no type annotation on `default_lookup`?

* `lookup` would return a `TypeError`
* This error wouldn't be caught in the `case` alternatives
* The error would bubble up to the surface.

```
> 0.default_lookup 1
Exception:
  In the expression `0.default_lookup 1`
  In the expression `dict.lookup key`
    Where key = `1`
    Where dict = `0`
    TypeError: 
      Function `lookup` expects an argument of type `{a => b}`
      Can't unify `{a => b}` with `Num`
```

But what if we just had a catchall `Error _`? Then we'd attempt to dereference the second internal type of a constant type (which has 0 internal types):

```
> 0.default_lookup 1
Exception:
  In the expression `0.default_lookup 1`
  In the case alternative for `Error _`
  In the expression `default: (dict\type 1)`
    Where dict = `0`
    Where dict\type = `Num`
  In the expression `dict\type 1`
    ParameterError:
      Expected an object with at least 2 parameters.
      Instead got `Num`, which has 0 parameters.
```


One thing that this mentions is the idea of type unions:

```
> my_list = [~ 'hello', 2]
> :t my_list
my_list : [~ {Str, Num}]
> list2 = 'yoyoyo!' ~ my_list
> :t list2
list2 : [~ {Str, Num}]
> bad = Just 'hey' ~ list2
Exception:
  In the expression `Just 'hey' ~ list2`
  TypeError:
    Can't unify the type `Maybe Str` with `{Str, Num}`
> maybes = [~ Just 3, Nothing, Just 1, 1]
> :t maybes
maybes : [~ {Maybe Num, Num}]
> maybes2 = [~ Just 3, Nothing, Just, 1]
> :t maybes2
maybes : [~ {Maybe Num, a -> Maybe a, Num}]
```

One issue with this though is:

```
> mut maybes = [Just 'hello', Just 1]
> :t maybes
maybes : [{Maybe Str, Maybe Num}]
> maybes.push! Nothing
> :t maybes
maybes : [{Maybe Str, Maybe Num}]
> :t maybes 2
maybes 2 : ???
```

What is `maybes 2`? We don't know, because there's no argument on `Nothing` to tell us what kind of `Maybe` it is. Perhaps we should simply require a cast. How about Monoid for maybes?

```
Monoid for (Maybe (a of Monoid)) =
{ empty = Nothing;
  m1.append m2 = case (m1, m2) of
    (Nothing, _) => m2 |
    (_, Nothing) => m1 |
    (Just a, Just b) => Just (a.append b) }
```

According to what we were thinking of before, `empty` requires a cast, because it's a stub. Similarly `Nothing`, or indeed any types with assumed parameters, should require casts?

Or could we simply resolve these at runtime? After all, if the object doesn't *contain* any parameters, then we won't have any chance of a failure occurring because of those parameters being the wrong type.

```
> foo = Nothing
> :t foo
foo : Maybe a
> bar = Just 3
> :t bar
bar : Maybe Num
> bar.append foo
Just 3
```

No problem there, because we're not using any data from the nothing, so it doesn't matter what it is. In fact, perhaps we could just do this:

```
> foo = Nothing
> :t foo
foo : Maybe _
```

This indicating that `foo` actually *has no* internal type. It could be cast to anything we want:

```
> bar = foo : Maybe Num
> baz = foo : Maybe {Str, Num, [Num]}
```

Just as a trait stub can be cast to anything.

One thing to ponder is whether the type `Maybe _` should be considered to be *distinct* from the type `Maybe Num`. For example, if `foo = [Just 1, Nothing]`, should `foo` be of type `[Maybe Num]`, or of type `[{Maybe Num, Maybe _}]`? Instinct says the former, but the latter would reflect the fact that we can safely perform the action `bar = foo 1 : Maybe Str`; i.e., the that `Nothing` can be cast to anything we want...?

* a type annotation in a function argument is an assertion.
* a type annotation in an expression is a cast
* at least initially, any cast might fail
  - unless we can guarantee at compile-time that the cast is unnecessary
  - initially, we don't need to do this

This might all be hideously loosey-goosey from a Haskell point of view, but it's very interesting from a dynamic-languages point of view!
