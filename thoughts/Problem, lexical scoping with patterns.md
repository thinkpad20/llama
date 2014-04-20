Problem: lexical scoping with patterns...

```
foo n = 
  try bar, baz = qux n
  catch PatternMatchError do
    bar, baz = (1, 2)
  blibber_mcblob.bar baz
```

->

```
foo = n => 
  try
    _val = qux n
    if (PatAssert (_val `IsTupleOf` 2))
      bar = _val 0      # These variables won't be visible outside of
      baz = _val 1      # the scope of their If-block!
      blibber_mcblob.bar baz
    else
      throw PatternMatchError
  catch _exc do
    if ("PatternMatchError" `IsConstrOf` _exc)
      _val = (1, 2)
      if (PatAssert (_val `IsTupleOf` 2))
        bar = _val 0   # And neither will these!
        baz = _val 1
      else
        throw PatternMatchError
    else
      throw PatternMatchError
  blibber_mcblob.bar baz
```

How about

```
if (PatAssert...)
  bar = ...
  baz = ...
  Export [bar, baz]
```

As in, bar and baz get "exported" up into their earlier scope, instead of being destroyed when the scope finishes.

Seems hacky, but might work...
