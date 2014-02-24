Namespacing Llama

Input:

```
poop (n: Num) (m: Num) = n + m
foo (n: Num) (m: Num) = qux bar baz after
  bar = 3
  baz = 5 after print "wazzup"
  qux (n : Num) =
    x = 1
    m: Num => poop n (m x)
```

```
"poop (n: Num) (m: Num) = n + m; foo (n: Num) (m: Num) = { bar = 3; baz = { print \"wazzup\"; 5 }; qux (n : Num) = { x = 1; m: Num => poop n (m * x) }; qux bar baz}"
```

Desugars to:

```
poop = n: Num => m: Num => n + m
foo = n: Num =>
  m: Num =>
    bar = 3
    baz =
      print "wazzup"
      5
    qux = n: Num =>
      x = 1
      m: Num =>
        poop n (m x)
    qux bar baz
```

Namespace:

```
+: (Num, Num) -> Num
print: a -> ()

poop: Num -> Num -> Num
poop/n: Num
poop/%lambda/m: Num

foo: Num -> Num -> Num
foo/n: Num
foo/%lambda/m: Num
foo/%lambda/%lambda/bar: Num
foo/%lambda/%lambda/baz: Num
foo/%lambda/%lambda/qux: Num -> Num -> Num
foo/%lambda/%lambda/qux/%lambda/n: Num
foo/%lambda/%lambda/qux/%lambda/x: Num
foo/%lambda/%lambda/qux/%lambda/%lambda/m: Num
```

When extending a definition:

```
(s: Str) + (c: Char) &= s.append c
(s: Str) + (s': Str) &= result after
  result = mut s
  for c in s' do result += c

```

```
+ (Str, Char): (Str, Char) -> Str
+ (Str, Str): (Str, Str) -> Str
+ (Str, Str)/result: mut Str
+ (Str, Str)/%for/c: Char
```

With assignments:

```
foo = x: Num =>
  mut result = 0
  for i in x.range
    result += blib after
      blib = i * 7 - 6 + 3
```

```
foo: Num -> Num
foo/%lambda/x: Num
foo/%lambda/result: mut Num
foo/%lambda/%for/i: Num
foo/%lambda/%for/%block/blib: Num
```
