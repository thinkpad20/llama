Note: one thing that pure derivation gives us (I'm sure most optimizing compilers must do it to some degree) is the ability to throw out unused values. For example:


```
fact = n if n < 0 => Error "Negative argument to fact"
     | 0, 1       => 1
     | n          => n * fact (n - 1)

fact 100000000000
println "Done!"
```

Since `fact 100...` is never used, and since we can derive that `fact` is pure, we know that we can skip it. If we didn't know that `fact` was pure, we'd have to calculate `fact 10000...` even though its result doesn't get used, because we might need its side-effects.
