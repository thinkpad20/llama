```haskell
data Type = TVar       !Name
          | TConst     !Name
          | TTuple     ![Type] !TKwargs
          | TApply     !Type !Type
          | TFunction  !Type !Type
          | TMod       !Mod !Type
          | TMultiFunc !TypeMap
          | TUnion     !TUnion
          deriving (Show, Eq, Ord)

data TUnion = TAny | TOneOf (S.Set Type)
```

When reading from JSON, we could potentially receive one of a few different types:

```
> my_json = {j
  "foo": "bar",
  "baz": {
    "hey": [1, 2, "yoyo"]
  }
}
> get_value = key => my_json key
> :t get_value
get_value: Str -> One of {Str, Num, Bool, [Json], Json}
> my_value = get_value "foo"
> :t my_value
my_value: Str
```

This suggests the need for runtime casting. IDK, I just want to create a situation which most closely mimics the feel of a dynamic language, while actually being statically typed. BUT I don't want to make it so that the static typing is essentially an afterthought. Need to thinking about this.
