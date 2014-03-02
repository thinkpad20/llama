We could imagine compiling somthing like this Llama

```
foo (vec: [a], pred: a -> Bool) = 
  for a in vec do if pred a then break "woohoo" else poop a
```

To something like this JavaScript

```javascript
var foo;
foo = function(vec, pred) {
  var _result = Nothing;
  for (var _iter = VectorIterator(vec); _iter.valid(); _iter.forward()) {
    var a = _iter.get();
    if (unBool(pred(a))) {
      _result = Just("woohoo");
      break;
    } else {
      poop(a);
    }
  }
  return _result;
};
```

Incidentally, is this the same?

```

object VectorIterator a = 
  VectorIterator [a]
  index = 0

valid (iter = VectorIterator vec) = iter.index < vec.length
get (iter = VectorIterator vec) = vec i.index
forward! (ref iter = VectorIterator _) = iter.index++

foo (vec: [a], pred: a -> Bool) = 
  iter = mut VectorIterator vec
  while iter.valid
    a = iter.get
    if pred a then break "woohoo"
    poop a
    iter.forward!
```
