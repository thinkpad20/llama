MOAR thoughts

References are still weird. We might want to really think about separating
references, values, and mutables and their combinations.

```
# just spitballing here

# pure map
map (f: a->b) (v: [a]) = fix v' after
  v' = mut []
  for a in v do v'.push! (f a)

# map that modifies the vector
map! (f: a -> b) (v: mut [a]) = v after
  for i in v.range do v{i} := f v{i}

# map that modifies elements in the vector
# need to make sure f doesn't change the elements' types, but that shouldn't
# be a danger, since we're not reassigning anything
map!! (f: mut a -> ?) (v: mut! [a]) = v after
  for a in v do f a

inc! (n: mut Num) = n after n := n + 1

foo = mut! [1,2,3]
print foo # [1,2,3]
foo.map!! inc!
print foo # [2,3,4]
```

an alternative to the hindley-milner style typing we have now is

```haskell
data Type = TVar Name
          | TConst Name
          | TContainer Name [Type]

tupleT = TContainer ""
optionT a = TContainer "Option" [a]
mapT key val = TContainer "Map" [key, val]

numMapT = mapT numT numT
optionNumMapT = optionT numMapT
```

This might be theoretically more weak than the HM-style, but also could be
more easy to manipulate and understand, and more in line with the pragmatic
philosophy we're going for. IDK.
