type Foo = Foo; Bar
type Bar = Foo; Bar
foo1 = Foo\Foo
foo2 = Bar\Foo
bar1 = Foo\Bar
bar2 = Bar\Bar

type Baz <: Bar = 
  Baz Int <: Bar
  Qux <: Foo

baz = Baz.Baz 10


```
type Foo (a: Int) (b: Str) with c: Float = 0.0
f = Foo 10 'hey' with c=10.0
println typeof(f)
type Bar <: Foo = 
  B1 <: Foo 0 'yoyo'
  B2 s <: Foo 1234 s with c=45.3
```

```javascript
function type_of(obj) {
  return obj.type;
}
var Foo$type = new LlamaType('Foo', ['Foo'], null, {c: 0.0});
var Foo = function(a) {
  return function(b) {
    return new LlamaObject(Foo$type, [a, b], null);
  }
}
var f = Foo($int(10))('hey').with({c: 10.0});
println(type_of(f));
var Bar$type = new LlamaType('Bar', Foo$type, {});
var B1 = new LlamaObject(Bar$type, [], Foo($int(0))('yoyo'));
var B2 = function(s) {
  return new LlamaObject(Bar$type, [s], Foo(1234)(s).with({c: 45.3}));
};
```
