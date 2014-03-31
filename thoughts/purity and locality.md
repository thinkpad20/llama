## More thoughts on purity and locality

* A function is local *iff*:
  - it does not capture any mutable variables from its enclosing scope
  - all functions it calls are local

```
# local, doesn't capture anything or call non-local functions
this_is_local (v: ref [a of Num]) (x: a) = v.push! (x.add 2)
```

* A function is pure *iff*:
  - it is local
  - it does not mutate any of its arguments which are references

```
# pure, because its creation and its calling do not generate side-effects
this_is_pure () = {
  mut i = 0;
  => {
    print "called #{i.incr!} times"
  }
}

# impure, because its calling will have a side-effect
this_isn't_pure = this_is_pure()
```

* An expression is pure *iff*:
  - it is the result of calling a pure function
  - if the expression is a block, all expressions within that block are either pure, or if local, only operate on variables inside the block

```
# pure (trivially)
3

# impure, because its creation has a side-effect
not_pure = 2 after print "hey!"

# pure
my_vec = vec after {mut vec = []; for i in 10.range do vec.push! i}
```

* A module is pure *iff*:
  - all functions called at the top level are pure (i.e. every top-level expression is pure)
  - all modules it imports are pure

--------------------------

We need to think about where we draw the line with capturing variables. After all, purity is all about scope. Within the scope of a pure function, we can have impure values (e.g. this is the principle the ST monad is based on). So we might be able to be smarter about it. Llama is *default impure*, and *optionally pure*. Haskell is *default pure*, and *optionally impure*.
