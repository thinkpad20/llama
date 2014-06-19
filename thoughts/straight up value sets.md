@index: 
  {String=>Int} -> String -> Int
  [String] -> Int -> String


Maybe it would be OK to do straight up value sets, instead of traits...


_+_: 
  Int -> Int -> Int
  Float -> Int -> Float
  Int -> Float -> Float
  Float -> Float -> Float

foo = 1 + 2

foo (x: a) : a with {_+_: a -> a -> a} = x -> x
foo: a -> a with {_+_: a -> a -> a} = x -> x

alias Monad m =
  lift: a -> m a
  bind: m a -> (a -> m b) -> m b

_>>=_: m a -> (a -> m b) -> m b with Monad m = bind

Constraints can either be satisfied by existence or by assumption...

Could we infer that, though? For example:

```
foo x = x[1]
#=> foo : a -> b with Index a Int b
```

How could we infer that? When we have `x[1]`, it desugars into `@index x 1`, so we'd need to be able to examine `@index` and get some meaning out of it. We could just guess entirely, but that would allow almost everything to type check (wouldn't it?). 
