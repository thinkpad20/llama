foo (n: Num) (m: Str) = show n + "Yay! #{m}"

bar (s: [Str]) = result after
  result = mut []
  foo = reverse s
  for (c: Char) in foo
    result += foo c.toInt 4

-- soooo.... here, it definitely seems like variables should
-- shadow functions. As in, 'foo' has no business being a function
-- when it has already been declared as a local variable with
-- a Str type. Blargh.

-- We could simply make a distinction between top-level and function-level
-- variables: say, a function can take on multiple argument types if and only
-- if those declarations are all made at top-level.

-- We could alternatively have separate syntax for 'adding' to an existing
-- function's accepted types...

foo (n: Num) = n + 1
foo (n: Num, m: Num) &= foo (n + m)

toUpper (c: Char) = ...
toUpper (s: Str)  = ...

bar (s: Str) = result after
  foo (s: Str) &= s.reverse.toUpper
  result = []
  for c in s.foo do
    n = c.toInt
    result += foo n

-- this is promising. So an `&=` would extend the current definition of a
-- variable (within the current scope), while an `=` would shadow any prior
-- definitions.

(x: a) + (v: [a]) = v.prepend x
(v: [a]) + (x: a) &= v.append x
(v: [a]) + (v': [a]) &= v.concat x

