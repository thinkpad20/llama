```
> trait @Call a b -> c =
.   @call: a -> b -> c
> @call (f: a -> b) (x: a) :== f x
> @call (v: [a]) (index: Int) :== v[: index]
> foo = [1,2,3,4,5]
> assert foo 1 == 2
> :t map foo
map foo : [Int] -> [Int]
```
