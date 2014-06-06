Could we do something like:

```
(f: a -> b) $ (x: a): b = f x
(f: a -> Int) $ (x: a): Int = 
  println 'we are making an Int!'
  f x
```

As in, make a specialized instance based on the type, at a more specific point than the outermost (in this case function)?
