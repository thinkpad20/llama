### A Few Definitions of Find

```
find (needle: a, haystack: [a]) = False after
  for a in v do if a == needle then return True
```

`for` can evaluate to a value, as long as `break` and `continue` are followed
by expressions:

```
find (needle: a, haystack: [a]) =
  for a in v do if a == needle then break True else continue False
```

We can use binary search

```
find (needle: a, haystack: [a]) = False after
  (start, stop) = (mut 0, mut haystack.length - 1)
  while start < stop
    case needle.compare (haystack (stop - start / 2)) of
      Greater => start := haystack (stop - start / 2) + 1
      Lesser => stop := haystack (stop - start / 2) - 1
      Equal => return True # could just as easily use `break true` here
```

How about recursion? How about smart recursion that doesn't make you
type in the same arguments multiple times?

```
find (needle: a, haystack: [a], @start=0, @end=haystack.length) =
  if start >= stop then False
  else case needle.compare (haystack (stop - start / 2)) of
      Greater => @rec (start = haystack (stop - start / 2) + 1)
      Lesser => @rec (stop = haystack (stop - start / 2) - 1)
      Equal => True
```

Note: it's really easy to see how the above function is easily convertible
to a loop... :)

Important to note, though, that `@rec` could lead to big problems in a curried function, so "use with caution." Of course, we should be able to detect most infinite loops pretty easily (called again with the exact same args in an otherwise-constant closure...).

```
bad_fact(n: Num) = f n after
  f (n: Num) (acc: Num) = if n < 2 then acc else @rec (n - 1) (acc * n)
```

In the above, we can immediately tell it's a failure because of the type system.