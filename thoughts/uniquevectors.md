```
object UniqueVector a <: [a]
  with elems: {s a} = empty

object UniqueArray a <: Array a
  with elems: {s! a} = empty

append! (ua : ref UniqueArray a) x = ua after
  if not ua\elems.contains x 
    ua\0.append! x
    ua\elems.add! x

append (ua : UniqueArray a) x =
  if ua\elems.contains x then ua 
  else UniqueArray (ua\0.append x) 
        with elems = ua\elems.add x
```

```
llama> foo = UniqueArray [a 1, 2, 3]
llama> foo.append! 4
[a 1, 2, 3, 4]
llama> foo
[a 1, 2, 3, 4]
llama> foo.append 5
[a 1, 2, 3, 4, 5]
llama> foo
[a 1, 2, 3, 4]
llama> foo.append! 4
[a 1, 2, 3, 4]
llama> foo.append! 5
[a 1, 2, 3, 4, 5]
```

We could do something like "if a function takes a `ref` as its first argument and would otherwise return `()`, return the `ref`." This might be kinda janky but it is what happens in the most common case.
