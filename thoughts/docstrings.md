Putting documentation in functions:

```
> foo (n: Int) (m: Int): Int =
.   #|Adds n and m together.|#
.   n + m
.
> @help foo
foo (n: Int) (m: Int): Int
Adds n and m together.
> foo : Str -> Int =
.   #|Returns the length of its argument.|#
.   length
.
> @help foo
foo (n: Int) (m: Int): Int 
Adds n and m together.
---
foo (s: Str): Int
Returns the length of s.
> foo a = 
.   #|The identity function.|#
.   a
.
> @help foo
foo (n: Int) (m: Int): Int 
Adds n and m together.
---
foo : Str -> Int
Returns the length of its argument.
---
foo (a: ?): ?
The identity function.
> foo (x: a) = 
.   #|The identity function - typed!|#
.   x
.
> @help foo
foo (n: Int) (m: Int): Int 
Adds n and m together.
---
foo : Str -> Int
Returns the length of its argument.
---
foo (x: a): a
The identity function - typed!
---
foo (a: ?): ?
The identity function.
```
