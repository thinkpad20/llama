Lazy functions:

```
b1: Bool && (lazy b2: Bool) = case b1 of
  True -> b2
  _ -> False

println (False && (-> throw $ Exception 'Uh oh!')()) # prints 'False'
```

Translation:

```javascript
var $and = $func();
$and.add_instance(Bool, function(b1) {
  return function(b2) {
    if (b1.constr_is('True')) {
      return b2.eval();
    } else {
      return False;
    }
  }
})
println($and(False)($Thunkify(function (_arg) {
  $throw(Exception($str('Oh crap!')));
}, $Tuple([]))));
```

### Idea:

We implement a very simple form of typing, which only has two types: `Strict` and `Lazy`. Then, for example, `plus` has type `Strict -> Strict -> Strict`, while `and` has type `Strict -> Lazy -> Strict`. (Presumably, the return type of any function will be strict, but possibly not). These are independent of the "actual" types (e.g. `Int`, `Str`, etc). We can therefore detect at compile-time that `and _` returns a function which takes a lazy argument. Given that, we can determine that we need to wrap the anonymous function in a thunk, which will prevent the error from being triggered.

We could for example write a no-op function:

```
no_op (lazy _) = 'too lazy to evaluate that!'
slow_fib n = case n of 0, 1 -> 1; n -> slow_fib (n-1) + slow_fib (n-2)
println 'Does nothing: #{no_op slow_fib(100000000)}'
# => Does nothing: too lazy to evaluate that!
```

Translation: (note that neither no_op nor slow_fib have type annotations, so they are presented as the "default" implementations of their names)

```javascript
var no_op = $func(function(_) {
  return $str('too lazy to evaluate that!');
});
var slow_fib = $func(function (n) {
  if (n.is_literal($int(0))) {
    return $int(1);
  } else if (n.is_literal($int(1))) {
    return $int(1);
  } else {
    return $add(slow_fib($minus(n)($int(1))))(slow_fib($minus(n)($int(2))));
  }
};
println($append($str('Does nothing: '))(no_op($Thunkify(slow_fib, $int(100000000)))));
```

Note that overloaded functions muddy the waters a little bit. However, I think a reasonable alternative for now is to say that once some argument has been determined to be lazy, it holds for all (further) uses of the function. So if you later wrote a version of `&&` which is not lazy, it would still be evaluated lazily.
