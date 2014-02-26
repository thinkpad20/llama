## Enforcing functional purity in Llama

Pure functions should always have the same output given the same input, and 
they should not have any side-effects. Example:

```
(s: Str) + (s': Str) = result after
  result = mut s
  for c in s' do result.push! c
```

We can see that the above has no side-effects. But it does call an impure function! But that function's side-effects are localized. Maybe we can envision a `local` designation:

```
local (++) (n: ref Num) = n := n + 1

pure count (pred: a -> Bool) (v: [a]) = result after
  result = mut 0
  for a in v do if pred a then ++result
```

So a `local` function:

1. May modify its arguments
2. May not modify any captured variables
3. Cannot perform IO

Then a `pure` function:

1. Cannot perform IO
2. Cannot capture variables which are mutable
3. Cannot modify references
4. Can only call other `pure` or `local` functions.

We should be able to algorithmically derive `pure` and `local` functions,meaning we'd only have to annotate it when we wanted to enforce it. (Note: it seems that there's a close relationship between pure/local functions and inlineable functions. This is something to think about.)

```
add (n: Num) (m: Num) = n + m

mwahaha (path: FilePath) = h.close after 
  (h = openW path).write "mwa ha ha"

pure good (n: Num) = n.add 3

pure bad(n:Num) = n.add 3 after mwahaha "foo.txt"
  
pure bad2 = { n = mut 0; () => n++ }

foo = mut 0
pure bad3 (n: Num) = n + foo

pure good2 () = { n = mut 0; () => n++ }
```

Compiling `good` would succeed even though `add` has not been declared as pure, because we derive its purity. However, `bad` would throw a compile-time error, because we can derive from `openW` and `write` that `mwahaha` is impure.

With `bad2`, it's impure because it returns a closure which captures a mutable variable. So calling `bad2(); bad2()` would end up with two different results. `bad3` is similarly bad because its output depends on `foo`, which might change. But `good2` is OK, because it always returns the same thing (even though what it returns isn't pure).

Need to thinking about this one for sure.