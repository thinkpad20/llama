```
# This one's done better /not/ as a trait, because we don't 
# know what the return type is going to be.
make_deep_mutable (vec: [a]) = Array vec
make_deep_mutable (map: {k => v}) = Hash map
```

```
> bar = mut! [1,2,3]
> # is the same as
> foo = mut make_deep_mutable [1, 2, 3]
> :t foo
foo: mut Array Num
> x = mut! 0
Error: `make_deep_mutable' is not defined to take a `Num'
> make_deep_mutable (n: Num) = n
> x = mut! 0
> # OK, kinda useless but no problem
```

Of course this doesn't really solve our "reference problem"...

## Desugaring `for`

**TL;DR** there's no need for checking a `for _ in _` loop in type checking. Everything a that loop does can be desugared into a `for _; _; _` loop. Let the type checking happen there.

```
for i in vec
  poop_pants i
```

Desugars to

```
for _iter = mut vec.iter; _iter.is_valid; _iter .= forward
  i = _iter.get
  poop_pants i
```

Tentatively, it looks like we don't need traits for a `for` loop; we can do it all through multiple dispatch. The `for _ in _` loop is a sugared `for _; _; _` loop, and if the appropriate methods aren't defined, or don't return the right types, we simply throw an error. We could even support destructuring:

```
result = mut 0
for (foo, bar) in vec
  result += foo.bar
```

Desugars to

```
result = mut 0
for _iter = mut vec.iter; _iter.is_valid; _iter .= forward
  _ref = _iter.get
  case _ref of
    (foo, bar) => result += foo.bar
```

This would also seem to support infinite data structures right out of the box:

```
object InputStreamIterator = ISI (handle: System/File, buffer: Str)

is_valid (isi: InputStreamIterator) = System/is_open isi.handle
get (isi: InputStreamIterator) = isi.buffer
forward (isi: InputStreamIterator) = 
  case System/read isi.handle of
    Nothing => 
      System/close isi.handle
      isi (buffer="")
    Just data =>
      isi (buffer=data)

iter (file: System/File) = ISI (file, "")

read (file: System/File) = buffer after
  buffer = mut ""
  for data in file do buffer += data

lines (file: System/File) = file.read.split "\n"

read_real_time (input_stream: System/File) = 
  buffer = mut ""
  # input_stream might never end, but we can read it forever.
  for data in input_stream
    buffer += data
    if buffer.length >= 1000
      print "Received some data: #[buffer]"
      buffer.clear!
```
