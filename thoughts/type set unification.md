ex: 
foo: {Int->Str, Str->Int, Char}
bar: {Int, Str, Str->Int}
∴ foo bar: {Str, Int}

Why? Because 

unify {Int->Str, Str->Int, Char} {Int -> a, Str -> a, (Str->Int) -> a}
  -> [{a => Str}, {a => Int}]

[{a => Str}, {a => Int}] •> a = {Str, Int}

Then 

```
unify :: TypeSet -> TypeSet -> TypeChecker [Substitution]
unify ts1 ts2 = fmap catMaybes $
  forM ts1 $ \t1 ->
    forM ts2 $ \t2 ->
      (Just <$> unify_ t1 t2) `catchError` \_ -> pure Nothing
```

Traits as shorthand:

```
trait Add a b c = _+_: a -> b -> c
```

Issue here is, what variables are quantified? What `a`, `b`, and `c` are we referring to? Each time we use `_+_`, we are referring to a different set of variables. In effect, `_+_` has no type on its own; it just refers to some (possibly empty) type set. For example

```
x -> x + x
```

Inferring this, `_+_` would resolve to an empty type set. This is a problem... urgh
