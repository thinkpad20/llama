```
shout s = s'.length after {s' = s + "!"; print s'}

length : {[a] -> Num, Str -> Num, [!a] -> Num}
+: {Num -> Num, (Num -> Num) -> Num, (Str, Char) -> Str, (Char, Str) -> Str, ([a], [a]) -> [a], ...}
print: a of Show -> Str
```

Can `shout`'s type be inferred? Its argument must be an argument type of `length`, so `[a]`, `Str`, or `[!a]`. And it must be one of the *first elements in the tuple of argument types of which are tuples* of `+`, and the second of which must be a `Str`. This means either `Num`, `Str`, or `Char`. And its return type must be able to be passed into `print`, which means that it must implement `Show`. 

All of this points to `Str -> Num`.

Let's just say for sake of craziness, that there's a version of `+` such as `(n: Num) + (s: Str) = (n.show + s).shuffle` which means it's of type `(Num, Str) -> Char`. And then let's say that for some reason we wrote a function `length (c: Char) = [c]`. Then if we called `shout 5`, the result would be either `['5']` or `['!']`. Meaning we'd have `shout: {Num -> [Char], Str -> Num}`. 

But! We also have `(v: [a]) + (x: a) = v.append x`, so we could pass `["hello", "world"]` in and we'd print `["hello", "world", "!"]` and return `3`. So we also have `shout: {Num -> [Char], Str -> Num, [Str] -> Num}`. Basically it gets super crazy if we actually try to infer everything. But, that doesn't mean it's impossible. Although with a sound foundation, it might simply follow from having the right inferrence rules.

Of course, if we want to *declare* a multiple-dispatch function, we need to give it its argument types, because otherwise we can't make that distinction. Right? Or not? Maybe it would work. But it sounds crazy.
