## Avoiding unecessary casts

Take a look at this translation:

```
bloop i = 
  try
    floop = i as Int
    if floop < 0 then floop else bloop (floop - 1)
  catch CastError
    0

function bloop(i) {
  try {
    var floop = i.cast(Int);
    return floop < 0 ? floop : bloop(floop - 1);
  } catch(_err) {
    if (_err.constrIs('CastError')) {
      return 0;
    } else {
      throw _err;
    }
  }
}

```

This is inefficient because in the recursive call, we *know* that the variable `floop` is an `Int`, so there's no need to cast each time. How about:

```
function bloop(i) {
  function $bloop(floop) {
    return floop < 0 ? floop : $bloop(floop - 1);
  }
  try {
    return $inner(i.cast(Int));
  } catch(_err) {
    if (_err.constrIs('CastError')) {
      return 0;
    } else {
      throw _err;
    }
  }
}
```

We might be able to do a similar thing for any function which guards the types of its inputs. The key thing is determining at which point we know with total certainty that a variable is a certain type, and optimizing future uses of it.

It's conceivable of course that we don't perform these operations, and leave it to the user to do so:

```
bloop i = 
  _bloop floop = if floop < 0 then floop else _bloop (floop - 1)
  try _bloop (i as Int)
  catch CastError try _bloop(i as Float)
  catch CastError do 0
```
