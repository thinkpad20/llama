Multiple dispatch as a relation:

The notation `f (x: Int): Int = y` adds the tuple `(Int, Int)` to the relation `R(f)`. If this is the first time `f` has been declared, `R` will be created as a new set. 

Keeping it informal:

```
f (x: Int):Int = x + 1 
# R(f) = {(Int, Int) => (x -> x + 1)}

f (x: a): [a] = [x]    
# R(f) = {(a, a) => (x -> [x]), (Int, Int) => (x -> x + 1)}

f (x: _): () = println 'bloops!'
# R(f) = {(a, a) => (x -> [x]), (Int, Int) => (x -> x + 1), (_, ()) => (() -> println 'bloops!')}

f (x:a): a = x
# R(f) = {(a, [a]) => (x -> [x]), (Int, Int) => (x -> x + 1), (_, ()) => (() -> println 'bloops!'), (a, a) => (x -> x)}

f (x: a) (list: List a): List a = list + x
# R(f) = {(a, [a]) => (x -> [x]), (Int, Int) => (x -> x + 1), (_, ()) => (() -> println 'bloops!'), (a, a) => (x -> x), (a, List a -> List a) -> (x -> list -> list + x)}

f (x:b): b = x
# ERROR: duplicate function signature

f: Maybe a -> a = 
  Nothing -> throw Error('No value to unpack')
  Just x -> x
# R(f) = {(a, [a]) => (x -> [x]), (Int, Int) => (x -> x + 1), (_, ()) => (() -> println 'bloops!'), (a, a) => (x -> x), (a, List a -> List a) -> (x -> list -> list + x), (Maybe a, a) -> (Nothing -> ... | Just x -> x)}

f: [a] -> a =
  [] -> throw Error('No value to unpack')
  vec -> vec[0]
# R(f) = {(a, [a]) => (x -> [x]), (Int, Int) => (x -> x + 1), (_, ()) => (() -> println 'bloops!'), (a, a) => (x -> x), (a, List a -> List a) -> (x -> list -> list + x), (Maybe a, a) => (Nothing -> ... | Just x -> x), ([a], a) => ([] -> ... | vec -> vec[0])}
```

Difficult typing:

```
foo: Int -> Str = show
foo: Float -> Int = floor ~> (- 1)
foo: Int -> Float = int_to_float ~> (- 1.0)
a = 1
b = foo a
println foo(b)
```

Steps:

```
a = 1 => a ~~ {Int, Float}
b = foo a => b ~~ {Str, Int, Float}
          => foo(b) ~~ Ambiguous!

foo b is ambiguous. Example:

  a = 1 : Float
  b = foo a = int_to_float 1 - 1.0 = 0.0
  foo(b) = floor 0.0 - 1 = -1

OR:
  
  a = 1 : Int
  b = foo a = show 1 = "1"
  

```
