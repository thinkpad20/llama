Some Llama expressions:

```
foo (x: Int): Int = x + 1
bar: Int -> Int = x -> x + 1
baz (x: Int) = x + 1
qux (x: Int) (y: Int): Int = x * y
blib x: Int -> Int = x + 1
blab (x: Int) (y: Int): Str = "#{x + y}"
bad = (x -> x + 1) : Int
```

Haskell representations:

```haskell
Define "foo" (Typed (Lambda "x" (Just Int) (x + 1)) Int)
Define "bar" (Typed (Lambda "x" Nothing (x + 1)) (Int -> Int))
Define "baz" (Lambda "x" (Just Int) (x + 1))
Define "qux" (Typed (Lambda "x" (Just Int) (Lambda "y" (Just Int) (x * y))) Int)
Define "blib" (Typed (Lambda "x" Nothing (x + 1)) (Int -> Int))
Define "blab" (Typed (Lambda "x" (Just Int) (Lambda "y" (Just Int) (show (x + y)))) Str)
Define "bad" (Typed (Lambda "x" Nothing (x + 1)) Int)
```

It seems there is value in including an optional type in the lambda expression. We just have to be aware of what we count as the type, because of course the lambdas are all functions, not Ints, so when we say `Typed (Lambda "x" (Just Int) (x + 1)) Int`, we really mean `Typed (Lambda "x" Nothing (x + 1)) (Int -> Int)`. We probably need to thinking about this.
