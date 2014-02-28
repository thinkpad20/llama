# Safe and unsafe lookups:
One option is to provide a default in the object, which will return if the lookup fails.

```
foo = {1 => 2, 3 => 4, @default=-1}
foo[:1] # == 2
foo[:3] # == 4
foo[:16] # == -1
```

Another is to provide a default in the lookup call:

```
object JSON =
  JObj {Str => JSON}
  JStr Str
  JNum Num
  JArr [JSON]
  JBool Bool
  JNull

bar = JObj { "hey" => JObj { "yo" => JStr "hi" }, "sup" => JNum 4 }
# syntactic sugar for json
assert bar == {j "hey": {"yo": "hi"}, "sup": 4 }
([:]): (JSON, Str) -> Maybe JSON
([:]): (JSON, Str, JSON) -> JSON
print bar[:"hey", {j}]    # prints {j "yo": "hi"}
print bar[:"hi"]          # throws KeyError
print bar[:"flippy", {j}] # prints {j}
```