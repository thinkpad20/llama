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
2. May not modify any captured variables, only arguments
3. Cannot perform IO

Then a `pure` function:

1. Cannot perform IO
2. Cannot capture variables which are mutable
3. If accepts references as arguments, cannot modify them
4. Can only call `local` functions if they operate on variables local to the function.

Note that every `pure` function is by definition also a `local` function, but not the other way around.

We should be able to algorithmically derive `pure` and `local` functions, meaning we'd only have to annotate it when we wanted to enforce it. This is nice because most of the time we don't care whether our functions are pure or not, and/or it's obvious from looking at it, so we don't want to have to go putting `pure` on everything that's pure, just in case we want to later on use it in a pure function that we *do* want to enforce purity in. It also means that, for example if we want to introduce logging for debugging, we only have to remove `pure` from top-level functions (if at all), rather than digging down and removing `pure` from tons of low-level functions.

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

Like `mut` and others, `pure` and `local` could be part of a function's type, so a function could be written to only accept pure functions, or whatnot. 

Another use of this is the idea of pure modules; that is, modules which are guaranteed not to have side-effects when they are loaded. This is useful because with top-level statements, we could for example try to contact a web-server in the middle of an import statement and hang, or we could have leftover print statements in some module. Sometimes this is exactly what is desired, but when it isn't, it can be frustrating. A pure module would have no IO performed at top-level (aside from other import statements, which of course would be pure themselves). The purity of non-IO functions at the top level of a pure module seems tentatively to be irrelevant.

Need to thinking about this one for sure.

Note: it seems that there's a close relationship between pure/local functions and inlineable functions. This is something to think about.
