```
@call: (a, _) -> Int -> a = (a, _) -> 0 -> a
@call: (_, b) -> Int -> b = (_, b) -> 1 -> b
```

This means that we can call a tuple of type `(a, b)` with an int. However, it's ambiguous what type we will get back, so we need to declare it explicitly when we use it, if it can't be inferred from the context.

```
assert ((1, 'hello') 0 : Num == 1)
assert ((1, 'hello') 1 : Str == 'hello')
```

Although this is a bit crappy, the nice thing is that now we can type check if we're asking for the wrong type:

```
foo: Char = (34, 'hey') 1
# ERROR: No method @call: (Int, Str) -> Int -> Char
```

Wait.... maybe we can't. Crud. Because if it were `Int` instead of `Char`, there would be such a method (it just blows up unless the index is 0). And dependent types are too heavy, so back to the drawing board on this one.

```
foo: (_, Str) -> Int = tup -> length (tup 1 : Str)
```
