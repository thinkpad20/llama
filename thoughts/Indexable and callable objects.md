## Indexable and callable objects

* `@index a b` is the desugared form of `a[b]`

```
trait Indexable a b with @index: Self -> a -> b
@index : [a] -> Int -> Ref a = <BUILTIN>
@index (vec: [a]) (i: Int): a = !(vec[a])
@index : {a=>b} -> a -> Ref b = <BUILTIN>
@index (dict: {a=>b}) (key: a): b = !(dict[key])
```

* `@call a b` is the desugared form of `a b`

```
trait Callable a b with @call: Self -> a -> b
@call: (a->b) -> a -> b = <BUILTIN>
@call (v: [a]) (i: Int): a = v[i]
@call (dict: {a=>b}) (key: a) = dict[key]
@call (i: Int) (j: Int) = i * j
@call: Int -> Float -> Float = (*)
```

One thing we can see with this:
```
foo = @call ['hello', 'world']   # foo : Int -> Str
bar = map foo [0]                # bar = ['hello']
baz = map ['hello`, 'world'] [1] # baz = ['world']
```

So there's no distinction between mapping `@call obj` and mapping `obj` itself, if `obj` is callable.

An important point though: `@call [1,2,3] =/= [1,2,3]`. Because once `@call` has been applied, it's strictly a function, and no longer an array. For example, we can't say `foo = @call [1,2,3]; println foo[1]`. An array can *act* like a function, but *not* the other way around. Although, we could write `@index (f: a->b) (x: a): b = f x`... this could introduce circularity though.
