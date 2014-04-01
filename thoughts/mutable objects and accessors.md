Objects with unnamed variables in their constructors can only use pattern matching to access their variables.

```
> object Foo = Foo Int
> mut foo = Foo 3
> print foo
Foo 3
> foo := Foo 4
> print foo
Foo 4
```

If their variables have names, we can access them directly with `\`, and assign them as well.

```
> object Bar = Bar (i: Int)
> mut bar = Bar 3
> print bar
Bar 3
> print bar\i
3
> bar\i := 4
> print bar
Bar 4
```

With multiple constructors, we can access them with `\`, but a runtime error (yikes!) will be thrown if the attribute doesn't exist in the given constructor.

```
> object Baz = {Baz (i: Int); Qux (s: Str)}
> mut baz = Baz 3
> print baz
Baz 3
> baz\i := 4
> print baz
Baz 4
> baz\s := "hello"
AttributeError: `Baz::Baz' has no attribute `i'
> baz := Qux "hey"
> print baz
Qux "hey"
> baz\s := "hello"
> print baz
Qux "hello"
```

Of course, these can be caught:

```
> mut baz = Baz 3
> try do baz\s := "hello"; catch _ {print "whoops!"; baz := Qux "hello"}
whoops!
> print baz
Qux "hello"
```

When variables are shared between constructors, they are always accessible:

```
> object Qux = {Blibber; Blobs Int; with bloop = "zup"}
> qux = Blibber
> print qux\bloop
zup
```

etc!
