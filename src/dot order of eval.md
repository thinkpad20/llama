foo(bar).baz(qux).blibber(blobs)

foo (baz bar) (blibber qux) blobs

foo (bar) baz

could have it that dotted precedence is higher than bare application, but lower than pParens....????

foo bar.baz qux.blibber blobs
  ==> foo (baz bar) (blibber qux) blobs

foo(bar).baz(qux).blibber(blobs)
  ==> (foo bar).(baz qux).(blibber blobs)

foo(bar, baz).qux(blibber, blobs)
  ==> foo (qux (bar, baz) (blibber, blobs))

as opposed to

foo(bar, baz).qux(blibber, blobs)
  ==> (foo (bar, baz)).(qux (blibber, blobs))

It seems annoying to write the above. IDK. Not a huge deal for now. Realistically if you wanted to represent the above, you could simply write

qux (blibber, blobs) (foo (bar, baz))

or

qux (blibber, blobs) $ foo (bar, baz)

Of course, an important consideration here is order of evaluation. When a dot is reversed application, does that mean the order of evaluation is also reversed? Does it matter? Let's see:

```
foo n = {print n; n + 1}
bar n = {print n; n - 1}

foo (bar 1) ==> prints 1, then 0
(bar 1).foo ==> prints 1, then 0
```

Actually I'm leaning towards thinking it doesn't matter with strict evaluation; there won't be any side-effects that aren't wrapped up in the functions themselves. HOWEVER:

```
mut i = 3;
foo = lazy {print "#{i := i + 5}"; j = i; n => n + j}
bar = lazy {print "#{i := i - 1}"; j = i; n => n * j}

bar 2 # prints 2, returns 4
foo 4 # prints 7, returns 11
i := 3
foo (bar 2)    # prints 8, then prints 7, returns 22
i := 3
2.bar.foo      # prints 2, then prints 7, returns 11
i := 3
(bar ~> foo) 2 # prints 2, then prints 7, returns 11
```

So with lazy evaluation, impure functions, and mutable variables, we need to worry about this stuff (i.e., we break referential transparency). I'm not sure if we need all three to cause problems, or if any of them are sufficient to cause this. Either way, though, it seems that we want to preserve order of evaluation as written, because although it might be terrible design, we need people to be able to reason about what's going to happen. So we don't want to desugar it to application. Yet.

We could also issue warnings when lazy values depend on mutable variables or impure functions, because that's clearly a recipe for disaster.
