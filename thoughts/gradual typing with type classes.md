```
trait Eq a = (==) : a -> a -> Bool

impl Eq Bool = b == b' = case (b, b') of
  (True, True) => True
  (False, False) => True
  _ => False
```

Given this, can we do gradual typing?

```
foo x = x == 3
```

Here, `foo` is not typed. However, we still know that Eq is a trait function, so we rewrite foo as

```
foo (==) x = x == 3
```

Then at runtime, we try to run `foo 1`, and we can see that there is no entry in the `Eq` dictionary for `Num`, and we throw a runtime error. Or, we could have written

```
foo : Eq a => a -> Bool
foo x = x == 3
```

Could we then detect at compile time that there's no instance for `Eq Num`? I think so... or even if we had this definition:

```
foo : Eq a => a -> a -> Int
foo x y = if x == y then 1 else 0

foo 1
```

Now we can tell at compile-time that this is an error, because there's no entry for `Eq` for `Num`...

This is promising! Although, it means that a lot of the work I've done will probably have to be scrapped :( if we do gradual typing that is. And who knows, maybe we'd rather not.
