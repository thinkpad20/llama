Blocks are like lambdas but a `return` from them constitutes a return from the surrounding function scope, not a return to the place from which they were called. But since they can take arguments and capture variables, they act like lambdas and can be called as such. They end up functioning like smart gotos.

```
parse (path: FilePath) = result after
  f = path.open
  result = SomeDataStructure
  block cleanup =
    f.close
  block handleError (msg: String) =>
    cleanup
    throw Exception "Error when calling `foo #{path}`: #[msg]"
  for line in f.lines
    ... some code here ...
    if (some unexpected condition)
      handleError "foo"
    for ...
      ... some more code here ...
      if (some other unexpected condition)
        handleError "bar"
  cleanup
```

Of course, with the above, we could do it with a function in python, since it throws an exception. The improvement with blocks is that they can also include `return`s. In python, a `return` from a function just takes you back to where it was called, but here, it can return you from your function early, and wrap up cleanup code at the same time. For example:

```
read_n_bytes (h: mut FileHandle, n: Int) = finish after
  result = mut ""
  block finish = return result after h.close
  for i in n.range
    if h.has_char then result += h.read_char! else finish
```

(note to self: much as I'm loathe to admit it, reviewing C++'s pass by value vs pass by reference is probably a good thing to do, since I'm thinking about something like that for Llama. Also similarly, we have the idea of const references vs mutable, etc).

Blocks would be very nice for separating out nested loops:

```
countCharsInLines (s: Str, c: Char) = counts after
  counts = mut []
  block getCounts (line: Str) = 
    count = mut {c => 0 | c in ['a'..'z']}
    for ch in line 
      if 'a' <= ch.lower <= 'z' then return []
      count[:ch.lower]++
    counts.push! (line, count)
  for line in s.splitLines do getCounts line
```