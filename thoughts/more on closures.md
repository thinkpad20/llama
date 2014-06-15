If a function is declared inside of another function, initialize it with a new type table. (E.g. initialize it as a new type set). Otherwise, don't give it any binding until absolutely necessary.

Ex:

```
f _ = g
#=> f: {g: b} => _ -> b
f 1
#=> Error: no `g` of type `a` defined
g = 3
f 1
#=> 3
f _ =
  g: Int -> Int = (* 3)
  g
#=> f: Int -> Int
g: Int -> Int = (+ 2)
f () 2
#=> 6 (ignores new definition of g)
g: Str -> Int = length
f () 'hello'
#=> Error: no function of type `Str -> a` in type set of `g`
```
