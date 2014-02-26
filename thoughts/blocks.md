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

Musing: should lambdas take a single statement, rather than a single expression? The reason I ask is that having that `return result after h.close` is pretty nice, and `return` is strictly a statement (need it be? we once again question the need to separate statements and expressions...)

An example of a lambda taking a statement:

```
dumb () = forever do println "hello!!"
```