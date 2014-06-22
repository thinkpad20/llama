```
foo = atomic 0
bar = ref 0
trait Update r = _:=_ : r a -> a -> a

_:=_ : Ref a -> a -> a = <BUILTIN>
_:=_ : Atomic a -> a -> a = 
  atom ->
    atom\lock.acquire
    a -> atom\contents := a before atom\lock.release

loop1 = -> 
  for _ in 100000.range
    foo += 1
    bar += 1

loop2 = -> 
  for _ in 100000.range
    foo -= 1
    bar -= 1

c1 = spawn loop1()
c2 = spawn loop2()

c1.join
c2.join
println *foo
println *bar
```
