```
print (x: a of Show) = Sys/stdout.write x.show
println (x: a of Show) = print "#{x}\n"

trait Show = show: Self -> Str

show (n: Num) = ...

show (s: Str) = '"' + result + '"' after
  result = mut ""
  for c in s do case c of
    '\n' => result += "\\n"
    '\t' => result += "\\t"
    '\r' => result += "\\r"
    '"'  => result += "\\\""
    '\\' => result += "\\\\"
    _    => result += c
```

```
_iter = mut s.iter
while _iter.valid
  c = _iter.get
  case c of ...
  _iter .= forward

split (s: Str, @onStr: Str, @onChar: Char)
  case (onStr, onChar) of
    (Just s, Just c) => throw Exception "Only onChar or onStr"
    (_, Just c) => s.splitOnChar c
    (Just s', _) => s.splitOnStr s'
    (_, _) => s.splitOnWhiteSpace
```

```
trait Functor a = map: (a -> b, Self a) -> Self b

map (f: a -> b, v: [a]) = result after
  result = mut []
  for x in v do result += f x

each (v: f of Functor a) (f: a -> b) = f `map` v
```

or...

```

trait Functor a = map: (a -> b) -> Self a -> Self b

map (f: a -> b) (v: [a]) = result after
  result = mut []
  for x in v do result += f x

```

Problem is `map` is not prefix-unique... Suggests that the "self type" must always come first...?

```

foo (x: box of Functor (item of Show)) = map show x

```
