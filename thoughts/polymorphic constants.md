string constants: can be chars, strings, char vectors, linked lists, etc

```
my_char: Char = 'c'
my_chars: [Char] = 'chars'

assert (my_chars 0 == my_char)
```

number constants: can be ints, floats, uints, fractions, etc.

```
my_int: Int = 1
my_float: Float = 1.0
```

Then how about polymorphic values?

```
my_num = 1
my_str = 'x'
```

Then are we able to do things like

```
my_int: Int = my_num
my_float: Float = my_num
my_char: Char = my_str
```

Or could we even do something like:

```
my_num = 1
my_num: Int = my_num + 1
```

See this gets complicated because now we're changing my_num's value depending on if it's read as an Int or as a Float. But maybe that's OK to a allow -- it just shouldn't be done in practice...
