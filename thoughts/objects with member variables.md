```
object List a =
  pattern Empty =
    size = 0
  pattern Cons (val: a) (next: List a) =
    size = next.size + 1

  shared size: Num

show (l: List (a of Show)) = case l of
  Empty => "[l]"
  Cons val rest => "[l" + result + "]" after
    result = mut val.show
    for runner = mut rest; runner != Empty; runner .= next
      result += ", " + runner.val.show

myList = Cons 1 (Cons 2 (Cons 3 Empty))
assert myList.size = 3
assert myList == [l 1, 2, 3]
assert myList.show == "[l 1, 2, 3]"
```


> mutVec = mut [1, 2, 3] # clojure-style
> mutArr = mut [a 1, 2, 3] # c-style
> mutList = mut [l 1, 2, 3] # linked list
> v += 4
> v
[1, 2, 3, 4]
> v 0
1
> v 2
3
> v.set! 0 5
> v
[5, 2, 3, 4]
> w = v.set 1 6
> w
[5, 6, 3, 4]
> v
[5, 2, 3, 4]
