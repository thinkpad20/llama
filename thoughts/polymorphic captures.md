```
module A exports bar
  foo: Int -> Int = (+ 1)
  bar: Str -> Int = .length.foo

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

Running this prints 4. The function `foo` is captured in the closure of `bar`, and `A.foo` is not exported, so there is no conflict with `B.foo`.

```
module A exports bar
  foo: Int -> Int = (+ 1)
  bar: Str -> Int | foo: Int -> Int = .length.foo

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

This time, it prints 9, because `bar` asks for *any* `foo` (of the given type),and since `A.foo` was not exported, `B.foo` is used.

```
module A exports bar
  foo: Int -> Int = (+ 1)
  bar: Str -> Int | foo: Int -> Int = .length.foo

module B
  openimport A
  println bar('hey')
```

This time we get an error, because `A.foo` was not captured, and so there is no `foo` to be called when `bar` is called.

```
module A exports foo, bar
  foo: Int -> Int = (+ 1)
  bar: Str -> Int | foo: Int -> Int = .length.foo

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

This time, we get an error because we are redefining `foo`, which was exported from A, so a `foo: Int -> Int` already exists.

```
module A exports bar
  bar: Str -> Int = s ->
    foo: Int -> Int = (+ 1)
    s.length.foo

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

This is fine (and prints 4). `foo` is captured by `bar` and the other `foo` has no impact. The definition of `foo` as an `Int -> Int` is only in the scope of `bar`.

```
module A exports bar
  bar: Str -> Int | foo: Int -> Int = s ->
    foo: Int -> Int = (+ 1)
    s.length.foo

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

This is fine and prints 4 *even though* `bar` is declared as taking any `foo`; this is because the `foo` used in `bar` is shadowed.

```
module A exports bar
  bar: Str -> Int | foo: Int -> Int = s ->
    f = foo
    foo: Int -> Int = (+ 1)
    s.length.foo.f

module B
  openimport A
  foo: Int -> Int = (* 3)
  println bar('hey')
```

This is fine and prints 12, because `f` grabs a copy of `foo` before `foo` is redefined to something else.
