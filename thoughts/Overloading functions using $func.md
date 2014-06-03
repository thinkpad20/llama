Translating overloading functions to JavaScript:

```
foo (x: Int): Int = x + 1
foo (s: Str): Str = reverse(s).concat '#{foo 13}'
println foo(10)
println foo('hello world!')
```

Translation:

```
var $func = function() {
  return function (_arg) {
    return this.get_instance(_arg.type)(_arg);
  };
};
var foo = $func();
foo.add_instance(Int, function(x) {
  return $add(x)($int(1));
});
foo.add_instance(Str, function(s) {
  return concat(reverse(s))(show(foo($int(13))));
});
println(foo($int(10)));
println(foo($str('hello world!')));
```
