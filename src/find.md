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

Of course, it's not clear what this would do if `haystack` were empty...

```
find (needle: a, haystack: [a]) =
  for a in v do if a == needle then break True else continue False
```

We could wrap them in a `Maybe`! That would mean that `break` would yield a `Just` value, and `continue` or finishing the loop a `Nothing`. `return` would work as normal.

```
find (needle: a, haystack: [a]) = result || False after
  result = for a in v, if a == needle then break True
```

This is more like the first definition we had, and essentially equivalent. However, it doesn't rely on `return`, and is more general then (for example, we use this in a variable assignment easily). We could do the same thing with `while`:

```
import sys

game (questions: [(Str, Int, @hint: Str)]) =
  score = mut 0
  quit = block
    print "Your score: #{score}"
    sys/exit 0
  ask (question: Str, answer: Int, @hint: Str) =
    forever 
      case prompt "Question: #[question]" of
        a if a.parseInt == Just answer => break True
        "exit" => quit "User exited"
        "hint" => case hint of
          Nothing => "There's no hint for this."
          Just hint => println hint
        _ => break False
  quit after
    for (q,a, h=@hint) in questions do case ask (q, a, @hint=h) of
      Just True => println "Correct! (#{++score} so far)"
      Just True => println "Wrong!"
      Nothing => throw Exception ()
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