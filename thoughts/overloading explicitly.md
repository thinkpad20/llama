Require named functions be typed if they are to be polymorphic.

```
foo (x: a): a =
  y = x
  z = y
  z
```

What we're essentially saying here, is that everything inside a function's scope has been defined either in its captures, in its arguments, or in its return type.

```
fact (n: Int): Int = case n of
  0, 1 -> 1
  _ -> n * fact n-1
```

How about trait functions?

```
lift (x: a): [a] = [x]
lift (x: a): List a = Cons x End
lift (x: a): Option a = Some a
```

Since we define them all in this way, we can use both the argument types AND the return types to create polymorphism. An anonymous function doesn't need this!

```
range (start: Int; stop: Option Int = None): List a = loop End start_ after
  start_, stop_ = case stop of None -> (0, start); Some n -> (start, n)
  loop acc n = case n of 
    'stop_ -> reverse acc
    _ -> loop (Cons n acc) n+1
```

We also shouldn't need to declare types of inner functions, because they're usually obvious.

