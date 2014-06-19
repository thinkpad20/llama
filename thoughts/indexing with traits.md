We'd like to have a general indexing trait, so that we can index both vectors and maps. However, this leads to the following problem:

```
foo = {'a'=>1, 'b'=>2}
println foo['a']   # then @index: {String=>Int} -> String -> Int

bar = ['hey', 'there']
println bar[0]     # then @index: [String] -> Int -> String
```

It appears that we want `@index` to work both on dicts (of kind `* -> * -> *`) and vectors (of kind `* -> *`).

But maybe it's alright? We just make `Index` parameterized by three types, object, key and value. Then we can say for example:

```
trait Index a b c = @index: a -> b -> c
# Adds an instance for [a] Int (Ref a)
@index (vec: [a]) (idx: Int): Ref a = <BUILTIN>
# Adds an instance for [a] Int a
@index (vec: [a]) (idx: Int): a = *(vec[idx])
# Adds an instance for {a=>b} a (Ref b)
@index (dict: {a=>b}) (key:a) : Ref b = <BUILTIN>
# Adds an instance for {a=>b} a b
@index (dict: {a=>b}) (key:a) : b = <BUILTIN>
```

