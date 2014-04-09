```
x = if foo then bar
else if baz then qux

foo = true -> x = Just bar
foo = false, baz = true -> x = Just qux
foo = false, baz = false -> x = Nothing

x = if foo then Just bar else if baz then Just qux else Nothing
```

so if a chain of if's doesn't end with an else, then append a `Just` to every

orrrr.... we just say that `if`s without elses always return ()...

```
if foo then bar else if baz then qux else buzz
```

We could have it so that it ignores typing if it's not in an assignment or the argument of an expression...? Or just do "lowest common denominator" typing, where if any of the branches return a `()`, then the whole thing is considered to be of type `()`.
