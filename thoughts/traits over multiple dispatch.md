```
trait Monoid a =
  zero : a
  append : a -> a -> a

impl Monoid Int = 
  zero = 0
  join n m = n + m

:t zero.join
zero.join : a -> a

assert zero.join 0 == 0
assert 0 == zero
assert 1.join 2 == 3
```

Maybe it would be better to have type classes than multiple dispatch? IDKKKKKKKKKKKKK

Yeah ya know in truth there prolly ain't much need for it...

```
trait Append a = 
  append: a b -> b -> a b
  prepend: a b -> b -> a b

impl Append [! a] = 
  prepend = flip (~)
  .append elem =
    x~xs => xs~xs.append elem
  | [!]  => [! elem]

assert [! 1, 2, 3].append 4 == [! 1, 2, 3, 4]
assert [! 1, 2, 3].prepend 4 == [! 4, 1, 2, 3]
```
