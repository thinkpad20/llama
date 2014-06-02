What would happen were we to gradually type the following:

```
implement Callable a b for {a => b} with
  @call dict key = !dict[key]

get (dict: {a => b}) (key: a; or: b) = 
  try dict key
  catch KeyError do case or of
    None -> throw
    Just b -> b
```

In the line `dict key`, we would get the constraint `Accepts ({a => b}) a`.Later when we are solving these constraints, we'd get the opportunity to see if this is a contradiction or not: in this case, it isn't, because we have an instance of Callable accepting any `a`, if `a` is the key type in the dictionary.

How about:

```
implement Callable Int a for [a] with
  @call vec key = !vec[key]

get (vec: [b]) (key: a; or: b) = 
  try dict key
    catch IndexError do case or of
      None -> throw
      Just b -> b
```

So this is almost the same, but this time we would get the constraint `Accepts [b] a`. As in Hindley-Milner, when we exit this function's scope, we'd see that `a` is for all `a`. But we don't have an instance of callable for *all* `a`; rather, only for an `Int`. So we'd get an error.

We could "fix" this by typing `key` as `?` (or not at all, which is the same semantic thing). The real fix, of course, would be to type it as `Int`.
