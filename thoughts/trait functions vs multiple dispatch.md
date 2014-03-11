## Trait functions vs Multiple Dispatch

The difference between trait functions and simple multiple dispatch is that the return types of trait functions are fixed. As in, it's not enough just to have a method which takes the right argument, but also it must return the right type, so that we can use it generically.

```
module Show
exports Show

trait Show = show: Self -> Str

show (s: Str) = '"' + result + '"' after
  result = mut ""
  for c in s do case c of
    '"' => result += "\\\""
    '\n' => result += "\\n"
    ...
show (n: Num) = ...
```

```
module Print
imports System, Show
exports print, println
print (x: a of Show) = System/stdout.write x.show
println (x: a of Show) = print "#{x}\n"
```
