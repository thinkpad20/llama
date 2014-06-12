With this declaration, there are many possible typings of `foo` that would work:

```
foo: Int -> Int = bliby bloopadoop
```

For example:

```
bloopadoop ~ Int -> Int
bliby ~ a -> a
```

OR

```
bloopadoop: Int
bliby ~ Int -> Int -> Int
```

Etc.

So are we screwed in trying to infer a type, or even type constraints?
Not if we specify the context. Because the most general type of `bliby bloopadoop` is `{bliby: a -> b, bloopadoop: a} => b`.

So what we need to say is

```
with bliby: (Int, Str) -> Int -> Int, bloopadoop: (Int, Str)
foo: Int -> Int = bliby bloopadoop
```

That way we can check if `foo` is consistent with its declared type and assumed context.

What if we already have a `bliby` and `bloopadoop` in mind?

