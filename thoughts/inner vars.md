## Accessing inner variables of functions

Given this source:

```
blorp = {
  /* does some crazy closure crap */
  blob = 2
  foo = n: Num => {
    /* does some stuff */
    bar x = x * 7; # completely self-contained
    baz x = n * (x + 1); # captures argument
    qux x = x - blob; # captures variable from earlier scope
    bar n + baz n + qux n
  }
  blob + foo 10
}
```

We can do this in the REPL:

```
> :t blorp
blorp : Num
> :t blorp\foo
blorp\foo : Num -> Num
> @doc blorp\foo
does some stuff
> :t blorp\foo\bar
blorp\foo\bar : Num -> Num
> blorp\foo\bar 5
35
> blorp\foo\qux 6
4
> foo 5
190
> :t blorp\foo\baz
blorp\foo\baz : Num -> Num
> blorp\foo\baz 10
Error: `foo\baz' has free variable `n' which is not in scope.
> [@doc blorp, @doc blorp\foo]
["does some crazy closure crap", "does some stuff"]
```

Basically if all of the variables a variable captures are in scope, we can
call it from outside the scope. Need to figure out the best namespace operator. `.` is ideal, but creates ugly ambiguity with existing `.`. Maybe `\` by default, and also some unicode character?
 
