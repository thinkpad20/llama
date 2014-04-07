OK so we're evaluating the following:

```
foo = n =>
  m = 32
  k => (m + k) / n

println foo 2 4
```

`foo` will return a closure; and we need that closure to contain the value of `m`. Also `foo` itself is a closure. We want `foo` to be `Closure (m = 32...) {n: Arg}`, and `foo 2` to return `Closure ((m + k) / n) {k: Arg, m: 32, n: 2}`

So how do we do that? We go through the AST. But one problem is that `n` is listed as being `Arg`, but we have a new arg: `k`. So when we complete the closure, we need to resolve the current argument. That means we need to look at the current frame's environment, which has

```
{m: 32, n: Arg}
```

And we add that to the new closure.

```
go (Apply (Apply / (Apply (Apply + m) k)) n)
```

Also we need to store any variables which get used in downstream lambdas, so...

