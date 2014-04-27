```
trait Reverse a = reverse: a -> a;
Reverse for Str = reverse = Builtins\str_reverse;
foo s = s.reverse + '!';
trait Lift f = lift : a -> f a;
Lift for Maybe = lift = Just;
bar = lift;
baz = bar 4;
Just bizz = baz
bloop = show ~> (<> '!')
```


```
Reverse = new Trait(TVar('a'), {'reverse': TFunction(TVar('a'))(TVar('a'))});
Reverse.add_instance('reverse', Str, Builtins.str_reverse);
function foo(s) {
  var reverse = Reverse.get_instance('reverse', s.type);
  var v1 = reverse(s);
  var _add = get_instance(Add, '+', v1.type);
  return _add(v1)('!');
}
Lift = new Trait(TVar('f'), {'lift', TFunction(TVar('a'))(TApply(TVar('f'))(TVar('a')))});
Lift.add_instance('lift', Maybe, Just);
var bar = Lift.duplicate('lift');
var bar = function(lift) {
  return lift;
}

// need to create something which is "waiting for a return type" in order to take its "actual form"
var baz = function (lift) {
  bar(lift)(4);
}

var _append = Append.get_instance('_append', '!'.type)
var v1 = function(_arg) {
  return _append(_arg, '!');
}

function bloop(show) {
  return function(_append) {
    return _rcomp(show)(function(_arg) {
      return _append(_arg)('!');
    })
  }
}

class Trait {
  constructor(tvar: Type, funcs: {[name: string]: Type}) {
    this.self_type = tvar;
    for (var i = 0; i)
  },
  funcs: {
    show: function(show_) {
      return show_;
    }
  }
}

Show = new Trait(TVar('a'), {
  show: TFunction(TVar('a'))(Str);
})


foo = map (1+) (lift 5)
println (foo: Maybe Num)
```

It would be great if we could create an object with a `cast` method, that could take a type and generate an object of that type... In fact this could conceivably work with any kind of cast, and then it would fail at runtime if the cast failed.

```
println(foo.cast(TApply(Maybe)(Num)))
```

```
foo (x: Num) = x + 1
```
Would generate

```
function foo(x) {
  x = x.cast(Num);
  return x + 1;
}
```

The thing is that *most* of these casts shouldn't be needed; we'd know if something is required.

```
trait JQuery j = {
  fadeIn: j -> j;
  fadeOut: j -> j;
  focus: j -> j;
  html: j -> Str;
  html: Str -> j;
  show: j -> j;
  addClass: j -> Str -> some JQuery;
  removeClass: Str -> some JQuery;
  append: HTMLElement -> some JQuery;
  val: j -> Str;
  val: j -> Str -> JQuery;
  attr(attrName: string): string;
}
```









trait Eq a = (==) : a -> a -> Bool;

```
function _eq(_a0) {
  return function(_a1) {
    var _u = unify(_a0, _a1);
    _a0 = _u[0];
    _a1 = _u[1];

  }
}
```

trait Join a = (<>) : a -> a -> a;

```
function _join (_a0) {
  return function(_a1) {
    var _u = unify(_a0, _a1);
    _a0 = _u[0];
    _a1 = _u[1];
    var _func = get_instance(Join, '<>', [_a0.type, _a1.type]);
    return _func(_a0)(_a1);
  }
}
```

trait Lift f = lift : a -> f a;
Lift for Maybe = lift = Just;
foo = lift 3
Just bar = foo: Maybe Num
```
function lift(a) {
  return Castable(Lift, a);
}
Lift = new Trait(TVar('f'), {
  lift: {
    type: TFunction(TVar('a'))(TApply(TVar('f'))(TVar('a'))),
    single: lift
  }
});
Lift.add_instance(Maybe, {lift: Just});
var foo = lift(3);
var bar;
if (is_constr(foo, 'Just')) {
  bar = foo.cast(TApply(Maybe)(Num)).deref('Just', 0);
} else {
  _throw(PatternMatchError());
}
```


















