```
foo = 2

bar x = foo * x ^ 2
```

need to encapsulate bar with its environment. We want foo by reference, not value. Because foo might later change.

```
{foo => VNum 2, bar => Closure (foo * x ^ 2)}
```

but when we're evaluating this later, we need to store it... for example

```
n = 3
m = 10

foo n = 
  o = n + 1
  m * o

bar = foo 7
bar() # Yo wazzup 8

baz k = 
  n = k + 1
  () => println "Yo wazzup #{n}"
```

Let's go through this. Let's say at the start the env contains `+`, `show` `append` and `println`. (So `println "Yo wazzup #{m}"` is really `println "Yo wazzup".append m.show`). And `foo n = ...` is really `foo = n => ...`

```
n = 3
m = 10
```

Namespace is root.
Env contains: `+, *, show, append, println, root\n, root\m`

```
foo = n =>
```

We push a new namespace `foo`, and a second one for its lambda.
Namespace is root\foo\%l
Env contains: `+, show, append, println, root\n, root\m, root\foo, root\foo\%l\n`

Note that the value that `root\foo\%l\n` maps to should be `Arg` or similar. "Whatever is the argument to my function, *n* is that argument." (Note: thinking about this, I realize that we might not want to go the namespace route, and instead go the symbol table route. But IDK.)

Next we have

```
  o = n + 1
```

... but wait! We can't evaluate this lambda yet because it hasn't been called. We need to compute the *closure* of the lambda. We need to find all of the variables that it creates and all of the variables it captures. In this case, it creates the variable `o`, and captures the variable `m`. We need to know going forward that it's referring to `root\m`, and not to some other `m`; for example:

```
qux k = 
  m = 45
  println (foo k) + m
```

In this case, we have a new binding for `m`, and we need to make sure it doesn't affect the binding of `m` that we had in `foo`.

So what we need to have is

```
Closure (Block [ Define "o", binary "+" (Var "n") (Number 1)
               , binary "*" (Var "m") (Var "o")])
        (Env {"m" => N "root\m", "+" => N "+", "*" => N "*", "n" => Arg})
```

As in, we're keeping track of every variable used here, so that we know that `m` and `n` refer to the proper variables.

What if we had

```
foo : (Int, Int) -> Int
foo = x => case x of
  (x, y) -> x + y
```

What would that look like?

```
Closure (Case (Var "x") 
            [(Tuple (Var "x", Var "y"), binary "+" (Var "x") (Var "y"))])
        (Env {"x" => Arg})
```

So the reassignment of the variables in the case statement is problematic. Let's think about that later...

So basically what it looks like we need to do is traverse the AST, find the full name of every variable in the AST, and make a dictionary of that...?

Let's just try that and wait to see what the better option is....
