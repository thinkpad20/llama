```
fact (n: Int) = case n of
  0, 1 -> 1
  n -> n * fact n-1
```

The one downside of Lua is that we don't get a `$` for name mangling :(. Also I don't know it as well. But otherwise, it's probably a lot better than JavaScript. Fortunately I don't think we need to do a huge amount to switch from one to the other (indeed, it would probably only be simpler), so let's stick with JS for now.

```lua
fact = __func__()
fact.add_instance(Int, function(n)
  if n.is_literal(0) then
    return __int__(1)
  else if n.is_literal(1) then
    return __int__(1)
  else
    return __mult__(n)(fact(__minus__(n)(1)))
  end
end)
```
