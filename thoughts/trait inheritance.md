```
trait Add a = (+) : a -> a -> a;

implement Add for Num with (+) = Builtin\add_num;
implement Add for Maybe (a of Num) with
  a + b = case (a, b) of
    (Nothing, _) => b
  | (_, Nothing) => a
  | (Just a', Just b') => Just (a' + b');

trait Numeric a = {Add a; Subtract a; Multiply a, negate: a -> a};
```

This might get translated to JavaScript as:

```

Add.add_instance(Num, {
  '+': Builtin._get('add_num');
});
Add.add_instance(Maybe a, {
  '+': function(a) {
    return function(b) {
      var _val = _Tuple([a, b], {});
      if (_val[0].is_constr('Nothing')) {
        return b;
      } else if (_val[1].is_constr('Nothing')) {
        return a;
      } else if (_val[0].is_constr('Just') && _val[1].is_constr['Just']) {
        var a_ = _val[0].deref('Just', 0);
        var b_ = _val[1].deref('Just', 0);
        return Just(_add(a_)(b_));
      }
    }
  }
});

Numeric = new Trait('a', {
    negate: ...
  }, 
  [Add('a', 'a', 'a'), ...]
)
```

The idea seems to be that you can "call" traits as functions (in JavaScript) to obtain new traits...? We need to think of how we could "look up" instances of inherited traits. Seems as simple as just looking in the dictionaries of any inherited traits.

```
implement Add for Num with (+) = Builtin\add_num;

implement Numeric for Num with {
  # Already have an instance for Add
  implement Subtract with (-) = Builtin\subtract_num;
  implement Multiply with (*) = Builtin\multiply_num;
  negate = Builtin\negate_num
}
```

What about multi-parameter type classes? Just spitballing here:

```
trait Add a b -> c = (+) : a -> b -> c;
```
