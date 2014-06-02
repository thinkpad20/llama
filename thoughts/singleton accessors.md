Both `Maybe` and `Ref` are singleton containers, and something like the `!` operator seems useful for both. If we were to overload it:

```
!(Ref a): a = a
!(m: Maybe a): a = case m of
  Some a -> a
  None -> throw Exception "No value in None"
```

How might this look in JavaScript?

```
var _bang = new $NamedFunction('!');

_bang.add_impl(type$Ref, function(_arg) {
  if (_arg.is_constr('Ref')) {
    var a = _arg.get_value('Ref', 0);
    return a;
  }
});

_bang.add_impl(type$Maybe, function(_arg) {
  if (_arg.is_constr('Just')) {
    var a = _arg.get_value('Just', 0);
    return a;
  } else if (_arg.is_constr('None')) {
    $throw(Exception($string('No value in None')));
  } else {
    $throw(PatternMatchError());
  }
});
```

By the way, what if `if` and `while` use a boolable trait or similar?

```
implement Boolable (Maybe _) with
  to_bool = None -> False
          | _    -> True
!(m: Maybe a): a = if m then m\0 else throw Exception "No value in None"
```
