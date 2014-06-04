Translating overloading functions to JavaScript:

```
foo a = [a]
foo (x: Int): Int = x + 1
foo (s: Str): Str = reverse(s).concat '#{foo 13}'
println foo(10)
println foo('hello world!')
```

Translation:

```
var $func = function(name, obj, f) {
  obj = obj || {};
  obj.instances = obj.instances || {};
  obj._default = obj._default || function () {
    $throw(Exception('No default implementation for function ' + name));
  }
  obj.has_instance = function(type) {
    if (!type) { return false; }
    return obj.instances[type.name] != null || obj.has_instance(type.parent);
  };
  obj.get_instance = function(type) {
    if (!type) { return obj._default; }
    return obj.instances[type.name] || obj.get_instance(type.parent);
  };
  obj.add_instance = function(type, func) {
    if (!type) {
      obj.instances._default = func;
    } else {
      obj.instances[type.name] = func;
    }
  };
  f = f || function (_arg) {
    return obj.get_instance(_arg.type)(_arg);
  };
  obj.__proto__ = f.__proto__;
  f.__proto__ = obj;
  return f;
};
var foo = $func('foo');
foo.add_instance(Int, function(x) {
  return $add(x)($int(1));
});
foo.add_instance(Str, function(s) {
  return concat(reverse(s))(show(foo($int(13))));
});
println(foo($int(10)));
println(foo($str('hello world!')));
```
